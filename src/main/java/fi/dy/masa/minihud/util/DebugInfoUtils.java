package fi.dy.masa.minihud.util;

import java.util.List;
import java.util.Map;
import java.util.function.Predicate;
import com.google.common.collect.MapMaker;
import com.mojang.blaze3d.systems.RenderSystem;
import io.netty.buffer.Unpooled;

import net.minecraft.client.MinecraftClient;
import net.minecraft.client.render.VertexConsumerProvider;
import net.minecraft.client.render.debug.DebugRenderer;
import net.minecraft.client.render.debug.NeighborUpdateDebugRenderer;
import net.minecraft.client.util.math.MatrixStack;
import net.minecraft.entity.Entity;
import net.minecraft.entity.LivingEntity;
import net.minecraft.entity.ai.pathing.EntityNavigation;
import net.minecraft.entity.ai.pathing.Path;
import net.minecraft.entity.mob.MobEntity;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.network.PacketByteBuf;
import net.minecraft.network.packet.s2c.common.CustomPayloadS2CPacket;
import net.minecraft.network.packet.s2c.custom.DebugPathCustomPayload;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.network.ServerPlayerEntity;
import net.minecraft.server.world.ServerWorld;
import net.minecraft.text.Text;
import net.minecraft.util.Formatting;
import net.minecraft.util.TypeFilter;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;

import fi.dy.masa.malilib.config.IConfigBoolean;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.mixin.IMixinEntityNavigation;

public class DebugInfoUtils
{
    private static boolean neighborUpdateEnabled;
    private static boolean pathfindingEnabled;
    private static int tickCounter;
    private static final Map<Entity, Path> OLD_PATHS = new MapMaker().weakKeys().weakValues().makeMap();

    public static void sendPacketDebugPath(MinecraftServer server, int entityId, Path path, float maxDistance)
    {
        DebugPathCustomPayload packet = new DebugPathCustomPayload(entityId, path, maxDistance);
        server.getPlayerManager().sendToAll(new CustomPayloadS2CPacket(packet));
    }

    private static Path copyPath(Path path)
    {
        PacketByteBuf buf = new PacketByteBuf(Unpooled.buffer());
        //path.toBuf(buf); // This won't work because the DebugNodeInfo is not set

        buf.writeBoolean(path.reachesTarget());
        buf.writeInt(path.getCurrentNodeIndex());
        buf.writeBlockPos(path.getTarget());

        int size = path.getLength();
        buf.writeVarInt(path.getLength());

        for (int i = 0; i < size; ++i)
        {
            path.getNode(i).write(buf);
        }

        buf.writeVarInt(0); // number of nodes in DebugNodeInfo
        buf.writeVarInt(0); // number of entries in openSet
        buf.writeVarInt(0); // number of entries in closedSet

        return Path.fromBuf(buf);
    }

    public static void onNeighborUpdate(World world, BlockPos pos)
    {
        // This will only work in single player...
        // We are catching updates from the server world, and adding them to the debug renderer directly
        if (neighborUpdateEnabled && world.isClient == false)
        {
            MinecraftClient mc = MinecraftClient.getInstance();
            mc.execute(() -> ((NeighborUpdateDebugRenderer) mc.debugRenderer.neighborUpdateDebugRenderer).addNeighborUpdate(world.getTime(), pos.toImmutable()));
        }
    }

    public static void onServerTickEnd(MinecraftServer server)
    {
        MinecraftClient mc = MinecraftClient.getInstance();

        // Send the custom packet with the Path data, if that debug renderer is enabled
        if (pathfindingEnabled && mc.world != null && ++tickCounter >= 10)
        {
            tickCounter = 0;
            ServerWorld world = server.getWorld(mc.world.getRegistryKey());

            if (world != null)
            {
                TypeFilter<Entity, MobEntity> filter = TypeFilter.instanceOf(MobEntity.class);
                Predicate<MobEntity> predicate = LivingEntity::isAlive;

                for (MobEntity entity : world.getEntitiesByType(filter, predicate))
                {
                    EntityNavigation navigator = entity.getNavigation();

                    if (navigator != null && isAnyPlayerWithinRange(world, entity, 64))
                    {
                        final Path path = navigator.getCurrentPath();

                        if (path == null)
                        {
                            continue;
                        }

                        Path old = OLD_PATHS.get(entity);
                        boolean isSamepath = old != null && old.equalsPath(path);

                        if (old == null || isSamepath == false || old.getCurrentNodeIndex() != path.getCurrentNodeIndex())
                        {
                            final int id = entity.getId();
                            final float maxDistance = Configs.Generic.DEBUG_RENDERER_PATH_MAX_DIST.getBooleanValue() ? ((IMixinEntityNavigation) navigator).getMaxDistanceToWaypoint() : 0F;

                            DebugInfoUtils.sendPacketDebugPath(server, id, path, maxDistance);

                            if (isSamepath == false)
                            {
                                OLD_PATHS.put(entity, copyPath(path));
                            }
                            else
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
        List<ServerPlayerEntity> players = world.getPlayers();
        double squaredRange = range * range;

        for (PlayerEntity player : players)
        {
            double distSq = player.squaredDistanceTo(entity.getX(), entity.getY(), entity.getZ());

            if (range < 0.0 || distSq < squaredRange)
            {
                return true;
            }
        }

        return false;
    }

    public static void toggleDebugRenderer(IConfigBoolean config)
    {
        if (config == RendererToggle.DEBUG_NEIGHBOR_UPDATES)
        {
            neighborUpdateEnabled = config.getBooleanValue();
        }
        else if (config == RendererToggle.DEBUG_PATH_FINDING)
        {
            pathfindingEnabled = config.getBooleanValue();
        }
        else if (config == RendererToggle.DEBUG_CHUNK_BORDER)
        {
            boolean enabled = MinecraftClient.getInstance().debugRenderer.toggleShowChunkBorder();
            debugWarn(enabled ? "debug.chunk_boundaries.on" : "debug.chunk_boundaries.off");
        }
        else if (config == RendererToggle.DEBUG_CHUNK_INFO)
        {
            MinecraftClient.getInstance().debugChunkInfo = config.getBooleanValue();
        }
        else if (config == RendererToggle.DEBUG_CHUNK_OCCLUSION)
        {
            MinecraftClient.getInstance().debugChunkOcclusion = config.getBooleanValue();
        }
    }

    private static void debugWarn(String key, Object... args)
    {
        MinecraftClient.getInstance().inGameHud.getChatHud().addMessage(Text.empty()
                .append(Text.translatable("debug.prefix").formatted(Formatting.YELLOW, Formatting.BOLD))
                .append(" ")
                .append(Text.translatable(key, args)));
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
