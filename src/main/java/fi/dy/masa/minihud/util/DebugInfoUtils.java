package fi.dy.masa.minihud.util;

import java.lang.reflect.Field;
import java.util.EnumSet;
import java.util.List;
import java.util.Map;
import java.util.function.Predicate;
import com.google.common.collect.MapMaker;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.debug.DebugRenderer;
import net.minecraft.client.renderer.debug.NeighborsUpdateDebugRenderer;
import net.minecraft.entity.Entity;
import net.minecraft.entity.MobEntity;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.network.PacketBuffer;
import net.minecraft.network.play.server.SCustomPayloadPlayPacket;
import net.minecraft.pathfinding.Path;
import net.minecraft.pathfinding.PathNavigator;
import net.minecraft.pathfinding.PathPoint;
import net.minecraft.server.MinecraftServer;
import net.minecraft.util.Direction;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraft.world.server.ServerWorld;
import fi.dy.masa.minihud.MiniHUD;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.RendererToggle;
import io.netty.buffer.Unpooled;
import net.minecraftforge.fml.common.ObfuscationReflectionHelper;

public class DebugInfoUtils
{
    private static final Field field_PathNavigator_maxDistanceToWaypoint = ObfuscationReflectionHelper.findField(PathNavigator.class, "field_188561_o"); // maxDistanceToWaypoint

    private static boolean neighborUpdateEnabled;
    private static boolean pathfindingEnabled;
    private static int tickCounter;

    private static final Map<Entity, Path> OLD_PATHS = new MapMaker().weakKeys().weakValues().<Entity, Path>makeMap();

    public static void sendPacketDebugPath(MinecraftServer server, int entityId, Path path, float maxDistance)
    {
        PacketBuffer buffer = new PacketBuffer(Unpooled.buffer());
        buffer.writeInt(entityId);
        buffer.writeFloat(maxDistance);
        writePathToBuffer(buffer, path);

        SCustomPayloadPlayPacket packet = new SCustomPayloadPlayPacket(SCustomPayloadPlayPacket.DEBUG_PATH, buffer);
        server.getPlayerList().sendPacketToAllPlayers(packet);
    }

    private static void writeBlockPosToBuffer(PacketBuffer buf, BlockPos pos)
    {
        buf.writeInt(pos.getX());
        buf.writeInt(pos.getY());
        buf.writeInt(pos.getZ());
    }

    private static void writePathPointToBuffer(PacketBuffer buf, PathPoint node)
    {
        buf.writeInt(node.x);
        buf.writeInt(node.y);
        buf.writeInt(node.z);

        buf.writeFloat(node.field_222861_j);
        buf.writeFloat(node.costMalus);
        buf.writeBoolean(node.visited);
        buf.writeInt(node.nodeType.ordinal());
        buf.writeFloat(node.distanceToTarget);
    }

    public static PacketBuffer writePathTobuffer(Path path)
    {
        PacketBuffer buffer = new PacketBuffer(Unpooled.buffer());
        writePathToBuffer(buffer, path);
        return buffer;
    }

    private static void writePathToBuffer(PacketBuffer buf, Path path)
    {
        // This is the path node the navigation ends on
        PathPoint destination = path.getFinalPathPoint();

        // This is the actual block the path is targeting. Not all targets
        // and paths will be the same. For example, a valid path (destination
        // in this case) to the the "meeting" POI can be up to 6 Manhattan
        // distance away from the target BlockPos; the actual POI.
        BlockPos target = path.func_224770_k();

        if (destination != null)
        {
            // Whether or not the destination is within the manhattan distance
            // of the target POI (the last param to PointOfInterestType::register)
            buf.writeBoolean(path.func_224771_h());
            buf.writeInt(path.getCurrentPathIndex());

            // There is a hash set of class_4459 prefixed with its count here, which
            // gets written to Path.field_20300, but field_20300 doesn't appear to be
            // used anywhere, so for now we'll write a zero so the set is treated as
            // empty.
            buf.writeInt(0);

            writeBlockPosToBuffer(buf, target);

            List<PathPoint> nodes = path.func_215746_d();
            PathPoint[] openSet = path.getOpenSet();
            PathPoint[] closedSet = path.getClosedSet();

            buf.writeInt(nodes.size());
            for (PathPoint point : nodes)
            {
                writePathPointToBuffer(buf, point);
            }

            buf.writeInt(openSet.length);

            for (PathPoint point : openSet)
            {
                writePathPointToBuffer(buf, point);
            }

            buf.writeInt(closedSet.length);

            for (PathPoint point : closedSet)
            {
                writePathPointToBuffer(buf, point);
            }
        }
    }

    public static void onNeighborNotify(World world, BlockPos pos, EnumSet<Direction> notifiedSides)
    {
        // This will only work in single player...
        // We are catching updates from the server world, and adding them to the debug renderer directly
        if (neighborUpdateEnabled && world.isRemote == false)
        {
            final long time = world.getGameTime();

            Minecraft.getInstance().execute(new Runnable()
            {
                public void run()
                {
                    for (Direction side : notifiedSides)
                    {
                        ((NeighborsUpdateDebugRenderer) Minecraft.getInstance().debugRenderer.neighborsUpdate).addUpdate(time, pos.offset(side));
                    }
                }
            });
        }
    }

    public static void onServerTickEnd(MinecraftServer server)
    {
        Minecraft mc = Minecraft.getInstance();

        // Send the custom packet with the Path data, if that debug renderer is enabled
        if (pathfindingEnabled && mc.world != null && ++tickCounter >= 10)
        {
            tickCounter = 0;
            ServerWorld world = server.getWorld(mc.world.dimension.getType());

            if (world != null)
            {
                Predicate<Entity> predicate = (entity) -> { return (entity instanceof MobEntity) && entity.isAlive(); };

                for (Entity entity : world.getEntities(null, predicate))
                {
                    PathNavigator navigator = ((MobEntity) entity).getNavigator();

                    if (navigator != null && isAnyPlayerWithinRange(world, entity, 64))
                    {
                        final Path path = navigator.getPath();
                        Path old = OLD_PATHS.get(entity);

                        if (path == null)
                        {
                            continue;
                        }

                        boolean isSamepath = old != null && old.isSamePath(path);

                        if (old == null || isSamepath == false || old.getCurrentPathIndex() != path.getCurrentPathIndex())
                        {
                            final int id = entity.getEntityId();
                            final float maxDistance = Configs.Generic.DEBUG_RENDERER_PATH_MAX_DIST.getBooleanValue() ? getPathPointWidth(navigator) : 0F;

                            DebugInfoUtils.sendPacketDebugPath(server, id, path, maxDistance);

                            if (isSamepath == false)
                            {
                                // Make a copy via a PacketBuffer... :/
                                PacketBuffer buf = DebugInfoUtils.writePathTobuffer(path);
                                OLD_PATHS.put(entity, Path.read(buf));
                            }
                            else if (old != null)
                            {
                                old.setCurrentPathIndex(path.getCurrentPathIndex());
                            }
                        }
                    }
                }
            }
        }
    }

    private static float getPathPointWidth(PathNavigator navigator)
    {
        try
        {
            return field_PathNavigator_maxDistanceToWaypoint.getFloat(navigator);
        }
        catch (Exception e)
        {
            MiniHUD.logger.warn("Failed to reflect PathNavigate#maxDistanceToWaypoint value", e);
        }

        return 0f;
    }

    private static boolean isAnyPlayerWithinRange(ServerWorld world, Entity entity, double range)
    {
        for (int i = 0; i < world.getPlayers().size(); ++i)
        {
            PlayerEntity player = world.getPlayers().get(i);

            double distSq = player.getDistanceSq(entity.posX, entity.posY, entity.posZ);

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
    }

    public static void renderVanillaDebug(long finishTime)
    {
        if (Configs.Generic.ENABLED.getBooleanValue() == false)
        {
            return;
        }

        DebugRenderer renderer = Minecraft.getInstance().debugRenderer;

        if (RendererToggle.DEBUG_COLLISION_BOXES.getBooleanValue())
        {
            renderer.collisionBox.render(finishTime);
        }

        if (RendererToggle.DEBUG_NEIGHBOR_UPDATES.getBooleanValue())
        {
            renderer.neighborsUpdate.render(finishTime);
        }

        if (RendererToggle.DEBUG_PATH_FINDING.getBooleanValue())
        {
            renderer.pathfinding.render(finishTime);
        }

        if (RendererToggle.DEBUG_SOLID_FACES.getBooleanValue())
        {
            renderer.solidFace.render(finishTime);
        }

        if (RendererToggle.DEBUG_WATER.getBooleanValue())
        {
            renderer.water.render(finishTime);
        }
    }
}
