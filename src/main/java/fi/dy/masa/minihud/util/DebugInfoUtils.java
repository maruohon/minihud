package fi.dy.masa.minihud.util;

import java.util.ArrayList;
import java.util.List;
import io.netty.buffer.Unpooled;
import net.minecraft.network.PacketBuffer;
import net.minecraft.network.play.server.SPacketCustomPayload;
import net.minecraft.pathfinding.Path;
import net.minecraft.pathfinding.PathPoint;
import net.minecraft.server.MinecraftServer;

public class DebugInfoUtils
{
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
}
