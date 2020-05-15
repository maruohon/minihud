package fi.dy.masa.minihud.util;

import java.util.EnumSet;
import java.util.List;
import java.util.Map;
import java.util.function.Predicate;
import com.google.common.collect.MapMaker;
import com.mojang.blaze3d.systems.RenderSystem;
import net.minecraft.client.MinecraftClient;
import net.minecraft.client.render.VertexConsumerProvider;
import net.minecraft.client.render.debug.DebugRenderer;
import net.minecraft.client.render.debug.NeighborUpdateDebugRenderer;
import net.minecraft.client.util.math.MatrixStack;
import net.minecraft.entity.Entity;
import net.minecraft.entity.ai.pathing.EntityNavigation;
import net.minecraft.entity.ai.pathing.Path;
import net.minecraft.entity.ai.pathing.PathNode;
import net.minecraft.entity.mob.MobEntity;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.network.PacketByteBuf;
import net.minecraft.network.packet.s2c.play.CustomPayloadS2CPacket;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.world.ServerWorld;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Direction;
import net.minecraft.world.World;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.mixin.IMixinEntityNavigation;
import io.netty.buffer.Unpooled;

public class DebugInfoUtils
{
    private static boolean neighborUpdateEnabled;
    private static boolean pathfindingEnabled;
    private static int tickCounter;
    private static final Map<Entity, Path> OLD_PATHS = new MapMaker().weakKeys().weakValues().<Entity, Path>makeMap();

    public static void sendPacketDebugPath(MinecraftServer server, int entityId, Path path, float maxDistance)
    {
        PacketByteBuf buffer = new PacketByteBuf(Unpooled.buffer());
        buffer.writeInt(entityId);
        buffer.writeFloat(maxDistance);
        writePathToBuffer(buffer, path);

        CustomPayloadS2CPacket packet = new CustomPayloadS2CPacket(CustomPayloadS2CPacket.DEBUG_PATH, buffer);
        server.getPlayerManager().sendToAll(packet);
    }

    private static void writeBlockPosToBuffer(PacketByteBuf buf, BlockPos pos)
    {
        buf.writeInt(pos.getX());
        buf.writeInt(pos.getY());
        buf.writeInt(pos.getZ());
    }

    private static void writePathPointToBuffer(PacketByteBuf buf, PathNode node)
    {
        buf.writeInt(node.x);
        buf.writeInt(node.y);
        buf.writeInt(node.z);

        buf.writeFloat(node.pathLength);
        buf.writeFloat(node.penalty);
        buf.writeBoolean(node.visited);
        buf.writeInt(node.type.ordinal());
        buf.writeFloat(node.heapWeight);
    }

    public static PacketByteBuf writePathTobuffer(Path path)
    {
        PacketByteBuf buffer = new PacketByteBuf(Unpooled.buffer());
        writePathToBuffer(buffer, path);
        return buffer;
    }

    private static void writePathToBuffer(PacketByteBuf buf, Path path)
    {
        // This is the path node the navigation ends on
        PathNode destination = path.getEnd();

        // This is the actual block the path is targeting. Not all targets
        // and paths will be the same. For example, a valid path (destination
        // in this case) to the the "meeting" POI can be up to 6 Manhattan
        // distance away from the target BlockPos; the actual POI.
        BlockPos target = path.getTarget();

        if (destination != null)
        {
            // Whether or not the destination is within the manhattan distance
            // of the target POI (the last param to PointOfInterestType::register)
            buf.writeBoolean(path.reachesTarget());
            buf.writeInt(path.getCurrentNodeIndex());

            // There is a hash set of class_4459 prefixed with its count here, which
            // gets written to Path.field_20300, but field_20300 doesn't appear to be
            // used anywhere, so for now we'll write a zero so the set is treated as
            // empty.
            buf.writeInt(0);

            writeBlockPosToBuffer(buf, target);

            List<PathNode> nodes = path.getNodes();
            PathNode[] openSet = path.method_22880();
            PathNode[] closedSet = path.method_22881();

            buf.writeInt(nodes.size());
            for (PathNode point : nodes)
            {
                writePathPointToBuffer(buf, point);
            }

            buf.writeInt(openSet.length);

            for (PathNode point : openSet)
            {
                writePathPointToBuffer(buf, point);
            }

            buf.writeInt(closedSet.length);

            for (PathNode point : closedSet)
            {
                writePathPointToBuffer(buf, point);
            }
        }
    }

    public static void onNeighborNotify(World world, BlockPos pos, EnumSet<Direction> notifiedSides)
    {
        // This will only work in single player...
        // We are catching updates from the server world, and adding them to the debug renderer directly
        if (neighborUpdateEnabled && world.isClient == false)
        {
            final long time = world.getTime();

            MinecraftClient.getInstance().execute(new Runnable()
            {
                public void run()
                {
                    for (Direction side : notifiedSides)
                    {
                        ((NeighborUpdateDebugRenderer) MinecraftClient.getInstance().debugRenderer.neighborUpdateDebugRenderer).addNeighborUpdate(time, pos.offset(side));
                    }
                }
            });
        }
    }

    public static void onServerTickEnd(MinecraftServer server)
    {
        MinecraftClient mc = MinecraftClient.getInstance();

        // Send the custom packet with the Path data, if that debug renderer is enabled
        if (pathfindingEnabled && mc.world != null && ++tickCounter >= 10)
        {
            tickCounter = 0;
            ServerWorld world = server.getWorld(mc.world.method_27983());

            if (world != null)
            {
                Predicate<Entity> predicate = (entity) -> { return (entity instanceof MobEntity) && entity.isAlive(); };

                for (Entity entity : world.getEntities(null, predicate))
                {
                    EntityNavigation navigator = ((MobEntity) entity).getNavigation();

                    if (navigator != null && isAnyPlayerWithinRange(world, entity, 64))
                    {
                        final Path path = navigator.getCurrentPath();
                        Path old = OLD_PATHS.get(entity);

                        if (path == null)
                        {
                            continue;
                        }

                        boolean isSamepath = old != null && old.equalsPath(path);

                        if (old == null || isSamepath == false || old.getCurrentNodeIndex() != path.getCurrentNodeIndex())
                        {
                            final int id = entity.getEntityId();
                            final float maxDistance = Configs.Generic.DEBUG_RENDERER_PATH_MAX_DIST.getBooleanValue() ? ((IMixinEntityNavigation) navigator).getMaxDistanceToWaypoint() : 0F;

                            DebugInfoUtils.sendPacketDebugPath(server, id, path, maxDistance);

                            if (isSamepath == false)
                            {
                                // Make a copy via a PacketBuffer... :/
                                PacketByteBuf buf = DebugInfoUtils.writePathTobuffer(path);
                                OLD_PATHS.put(entity, Path.fromBuffer(buf));
                            }
                            else if (old != null)
                            {
                                old.setCurrentNodeIndex(path.getCurrentNodeIndex());
                            }
                        }
                    }
                }
            }
        }
    }

    private static boolean isAnyPlayerWithinRange(ServerWorld world, Entity entity, double range)
    {
        for (int i = 0; i < world.getPlayers().size(); ++i)
        {
            PlayerEntity player = world.getPlayers().get(i);

            double distSq = player.squaredDistanceTo(entity.getX(), entity.getY(), entity.getZ());

            if (range < 0.0D || distSq < range * range)
            {
                return true;
            }
        }

        return false;
    }

    public static void toggleDebugRenderer(RendererToggle config)
    {
        if (config == RendererToggle.DEBUG_NEIGHBOR_UPDATES)
        {
            neighborUpdateEnabled = config.getBooleanValue();
        }
        else if (config == RendererToggle.DEBUG_PATH_FINDING)
        {
            pathfindingEnabled = config.getBooleanValue();
        }
        else if (config == RendererToggle.DEBUG_CHUNK_INFO)
        {
            MinecraftClient.getInstance().debugChunkInfo = config.getBooleanValue();
        }
        else if (config == RendererToggle.DEBUG_CHUNK_OCCLUSION)
        {
            MinecraftClient.getInstance().debugChunkOcculsion = config.getBooleanValue();
        }
    }

    public static void renderVanillaDebug(MatrixStack matrixStack, VertexConsumerProvider.Immediate vtx,
            double cameraX, double cameraY, double cameraZ)
    {
        DebugRenderer renderer = MinecraftClient.getInstance().debugRenderer;

        if (RendererToggle.DEBUG_COLLISION_BOXES.getBooleanValue())
        {
            renderer.collisionDebugRenderer.render(matrixStack, vtx, cameraX, cameraY, cameraZ);
        }

        if (RendererToggle.DEBUG_NEIGHBOR_UPDATES.getBooleanValue())
        {
            renderer.neighborUpdateDebugRenderer.render(matrixStack, vtx, cameraX, cameraY, cameraZ);
        }

        if (RendererToggle.DEBUG_PATH_FINDING.getBooleanValue())
        {
            renderer.pathfindingDebugRenderer.render(matrixStack, vtx, cameraX, cameraY, cameraZ);
        }

        if (RendererToggle.DEBUG_SOLID_FACES.getBooleanValue())
        {
            RenderSystem.enableDepthTest();
            renderer.blockOutlineDebugRenderer.render(matrixStack, vtx, cameraX, cameraY, cameraZ);
        }

        if (RendererToggle.DEBUG_WATER.getBooleanValue())
        {
            renderer.waterDebugRenderer.render(matrixStack, vtx, cameraX, cameraY, cameraZ);
        }
    }
}
