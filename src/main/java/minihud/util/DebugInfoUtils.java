package minihud.util;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import com.google.common.collect.MapMaker;
import io.netty.buffer.Unpooled;

import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.debug.DebugRendererNeighborsUpdate;
import net.minecraft.entity.Entity;
import net.minecraft.entity.EntityLiving;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.network.PacketBuffer;
import net.minecraft.network.play.server.SPacketCustomPayload;
import net.minecraft.pathfinding.Path;
import net.minecraft.pathfinding.PathNavigate;
import net.minecraft.pathfinding.PathPoint;
import net.minecraft.server.MinecraftServer;
import net.minecraft.world.World;

import malilib.util.game.WorldUtils;
import malilib.util.game.wrap.EntityWrap;
import malilib.util.game.wrap.GameUtils;
import malilib.util.position.BlockPos;
import minihud.config.Configs;
import minihud.config.RendererToggle;
import minihud.mixin.debugrenderer.DebugRendererMixin;
import minihud.mixin.debugrenderer.path_finding.PathNavigateMixin;

public class DebugInfoUtils
{
    private static boolean neighborUpdateEnabled;
    private static boolean pathFindingEnabled;
    private static int tickCounter;
    private static final Map<Entity, Path> OLD_PATHS = new MapMaker().weakKeys().weakValues().makeMap();
    private static final List<NeighborUpdate> NEIGHBOR_UPDATES = new ArrayList<>();

    public static void sendPacketDebugPath(MinecraftServer server, int entityId, Path path, float maxDistance)
    {
        PacketBuffer buffer = new PacketBuffer(Unpooled.buffer());
        buffer.writeInt(entityId);
        buffer.writeFloat(maxDistance);
        writePathToBuffer(buffer, path);

        SPacketCustomPayload packet = new SPacketCustomPayload("MC|DebugPath", buffer);
        server.getPlayerList().sendPacketToAllPlayers(packet);
    }

    private static void writePathPointToBuffer(PacketBuffer buf, PathPoint point)
    {
        buf.writeInt(point.x);
        buf.writeInt(point.y);
        buf.writeInt(point.z);

        buf.writeFloat(point.distanceFromOrigin);
        buf.writeFloat(point.cost);
        buf.writeFloat(point.costMalus);
        buf.writeBoolean(point.visited);
        buf.writeInt(point.nodeType.ordinal());
        buf.writeFloat(point.distanceToTarget);
    }

    public static PacketBuffer writePathToBuffer(Path path)
    {
        PacketBuffer buffer = new PacketBuffer(Unpooled.buffer());
        writePathToBuffer(buffer, path);
        return buffer;
    }

    private static void writePathToBuffer(PacketBuffer buf, Path path)
    {
        PathPoint target = path.getFinalPathPoint(); // FIXME is this the target?

        if (target != null)
        {
            buf.writeInt(path.getCurrentPathIndex());

            writePathPointToBuffer(buf, target);

            int countTotal = path.getCurrentPathLength();
            List<PathPoint> openSet = new ArrayList<>();
            List<PathPoint> closedSet = new ArrayList<>();
            List<PathPoint> allSet = new ArrayList<>();

            for (int i = 0; i < countTotal; i++)
            {
                PathPoint point = path.getPathPointFromIndex(i);

                if (point.nodeType.getPriority() < 0F)
                {
                    closedSet.add(point);
                }
                else if (point.nodeType.getPriority() > 0F)
                {
                    openSet.add(point);
                }

                allSet.add(point);
            }

            buf.writeInt(allSet.size());

            for (PathPoint point : allSet)
            {
                writePathPointToBuffer(buf, point);
            }

            buf.writeInt(openSet.size());

            for (PathPoint point : openSet)
            {
                writePathPointToBuffer(buf, point);
            }

            buf.writeInt(closedSet.size());

            for (PathPoint point : closedSet)
            {
                writePathPointToBuffer(buf, point);
            }
        }
    }

    public static void onNeighborNotify(BlockPos pos, long worldTime)
    {
        // This will only work in single player...
        // We are catching updates from the server world, and adding them to the debug renderer directly
        if (neighborUpdateEnabled)
        {
            NEIGHBOR_UPDATES.add(new NeighborUpdate(pos.toLong(), worldTime));
        }
    }

    public static void updateDebugRenderersOnServerTickEnd(MinecraftServer server)
    {
        // Send the custom packet with the Path data, if that debug renderer is enabled
        if (pathFindingEnabled)
        {
            addPathFindingDebug(server);
        }

        if (neighborUpdateEnabled)
        {
            Minecraft mc = GameUtils.getClient();
            List<NeighborUpdate> list = new ArrayList<>(NEIGHBOR_UPDATES);
            NEIGHBOR_UPDATES.clear();

            mc.addScheduledTask(() -> {
                DebugRendererNeighborsUpdate renderer = (DebugRendererNeighborsUpdate) mc.debugRenderer.neighborsUpdate;

                for (NeighborUpdate update : list)
                {
                    renderer.addUpdate(update.time, update.getPos());
                }
            });
        }
    }

    private static void addPathFindingDebug(MinecraftServer server)
    {
        if (++tickCounter >= 10)
        {
            tickCounter = 0;
            World world = WorldUtils.getServerWorldForClientWorld();

            if (world != null)
            {
                boolean addMaxDist = Configs.Generic.PATH_FINDING_DEBUG_POINT_WIDTH.getBooleanValue();

                for (Entity entity : world.loadedEntityList)
                {
                    if (entity instanceof EntityLiving)
                    {
                        addEntityPath((EntityLiving) entity, world, addMaxDist, server);
                    }
                }
            }
        }
    }

    private static void addEntityPath(EntityLiving entity, World world, boolean addMaxDist, MinecraftServer server)
    {
        PathNavigate navigator = entity.getNavigator();

        if (navigator == null || navigator.getPath() == null ||
            isAnyPlayerWithinRange(world, entity, 64) == false)
        {
            return;
        }

        Path path = navigator.getPath();
        Path old = OLD_PATHS.get(entity);
        boolean isSamePath = old != null && old.isSamePath(path);

        if (old == null || isSamePath == false || old.getCurrentPathIndex() != path.getCurrentPathIndex())
        {
            final int id = entity.getEntityId();
            final float maxDistance = addMaxDist ? ((PathNavigateMixin) navigator).minihud_getMaxDistanceToWaypoint() : 0F;

            DebugInfoUtils.sendPacketDebugPath(server, id, path, maxDistance);

            if (isSamePath == false)
            {
                // Make a copy via a PacketBuffer... :/
                PacketBuffer buf = DebugInfoUtils.writePathToBuffer(path);
                OLD_PATHS.put(entity, Path.read(buf));
            }
            else if (old != null)
            {
                old.setCurrentPathIndex(path.getCurrentPathIndex());
            }
        }
    }

    private static boolean isAnyPlayerWithinRange(World world, Entity entity, double range)
    {
        for (int i = 0; i < world.playerEntities.size(); ++i)
        {
            EntityPlayer player = world.playerEntities.get(i);

            double distSq = player.getDistanceSq(EntityWrap.getX(entity), EntityWrap.getY(entity), EntityWrap.getZ(entity));

            if (range < 0.0D || distSq < range * range)
            {
                return true;
            }
        }

        return false;
    }

    public static void toggleDebugRenderer(RendererToggle config)
    {
        Minecraft mc = GameUtils.getClient();
        boolean enabled = config.isRendererEnabled();

        if (config == RendererToggle.DEBUG_BLOCK_COLLISION_BOXES)
        {
            ((DebugRendererMixin) mc.debugRenderer).minihud_setCollisionBoxEnabled(enabled);
        }
        else if (config == RendererToggle.DEBUG_HEIGHT_MAP)
        {
            ((DebugRendererMixin) mc.debugRenderer).minihud_setHeightMapEnabled(enabled);
        }
        else if (config == RendererToggle.DEBUG_BLOCK_NEIGHBOR_UPDATES)
        {
            ((DebugRendererMixin) mc.debugRenderer).minihud_setNeighborsUpdateEnabled(enabled);
            neighborUpdateEnabled = enabled;
        }
        else if (config == RendererToggle.DEBUG_PATH_FINDING)
        {
            ((DebugRendererMixin) mc.debugRenderer).minihud_setPathfindingEnabled(enabled);
            pathFindingEnabled = enabled;
        }
        else if (config == RendererToggle.DEBUG_BLOCK_SOLID_FACES)
        {
            ((DebugRendererMixin) mc.debugRenderer).minihud_setSolidFaceEnabled(enabled);
        }
        else if (config == RendererToggle.DEBUG_WATER)
        {
            ((DebugRendererMixin) mc.debugRenderer).minihud_setWaterEnabled(enabled);
        }
    }

    private static class NeighborUpdate
    {
        private final long posLong;
        private final long time;

        public NeighborUpdate(long posLong, long time)
        {
            this.posLong = posLong;
            this.time = time;
        }

        public BlockPos getPos()
        {
            return BlockPos.fromPacked(this.posLong);
        }
    }
}
