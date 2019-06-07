package fi.dy.masa.minihud.util;

import java.util.ArrayList;
import java.util.EnumSet;
import java.util.List;
import java.util.Map;
import com.google.common.collect.MapMaker;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.mixin.IMixinDebugRenderer;
import fi.dy.masa.minihud.mixin.IMixinPathNavigate;
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
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;

public class DebugInfoUtils
{
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

        SPacketCustomPayload packet = new SPacketCustomPayload(SPacketCustomPayload.DEBUG_PATH, buffer);
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

    public static PacketBuffer writePathTobuffer(Path path)
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
            List<PathPoint> openSet = new ArrayList<PathPoint>();
            List<PathPoint> closedSet = new ArrayList<PathPoint>();
            List<PathPoint> allSet = new ArrayList<PathPoint>();

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

    public static void onNeighborNotify(World world, BlockPos pos, EnumSet<EnumFacing> notifiedSides)
    {
        // This will only work in single player...
        // We are catching updates from the server world, and adding them to the debug renderer directly
        if (neighborUpdateEnabled && world.isRemote == false)
        {
            final long time = world.getGameTime();

            Minecraft.getInstance().addScheduledTask(new Runnable()
            {
                public void run()
                {
                    for (EnumFacing side : notifiedSides)
                    {
                        ((DebugRendererNeighborsUpdate) Minecraft.getInstance().debugRenderer.neighborsUpdate).addUpdate(time, pos.offset(side));
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
            World world = server.getWorld(mc.world.dimension.getType());

            if (world != null)
            {
                for (Entity entity : world.loadedEntityList)
                {
                    PathNavigate navigator = entity instanceof EntityLiving ? ((EntityLiving) entity).getNavigator() : null;

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
                            final float maxDistance = Configs.Generic.DEBUG_RENDERER_PATH_MAX_DIST.getBooleanValue() ? ((IMixinPathNavigate) navigator).getMaxDistanceToWaypoint() : 0F;

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

    private static boolean isAnyPlayerWithinRange(World world, Entity entity, double range)
    {
        for (int i = 0; i < world.playerEntities.size(); ++i)
        {
            EntityPlayer player = world.playerEntities.get(i);

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
        Minecraft mc = Minecraft.getInstance();
        boolean enabled = config.getBooleanValue();

        if (config == RendererToggle.DEBUG_COLLISION_BOXES)
        {
            ((IMixinDebugRenderer) mc.debugRenderer).setCollisionBoxEnabled(enabled);
        }
        else if (config == RendererToggle.DEBUG_HEIGHT_MAP)
        {
            // This crashes in 1.13+, because it uses a world gen heightmap which doesn't exist normally
            //((IMixinDebugRenderer) mc.debugRenderer).setHeightMapEnabled(enabled);
        }
        else if (config == RendererToggle.DEBUG_NEIGHBOR_UPDATES)
        {
            ((IMixinDebugRenderer) mc.debugRenderer).setNeighborsUpdateEnabled(enabled);
            neighborUpdateEnabled = enabled;
        }
        else if (config == RendererToggle.DEBUG_PATH_FINDING)
        {
            ((IMixinDebugRenderer) mc.debugRenderer).setPathfindingEnabled(enabled);
            pathfindingEnabled = enabled;
        }
        else if (config == RendererToggle.DEBUG_SOLID_FACES)
        {
            ((IMixinDebugRenderer) mc.debugRenderer).setSolidFaceEnabled(enabled);
        }
        else if (config == RendererToggle.DEBUG_WATER)
        {
            ((IMixinDebugRenderer) mc.debugRenderer).setWaterEnabled(enabled);
        }
    }
}
