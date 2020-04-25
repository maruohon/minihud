package fi.dy.masa.minihud.renderer;

import net.minecraft.client.MinecraftClient;
import net.minecraft.client.render.BufferBuilder;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Vec3d;
import fi.dy.masa.malilib.util.Color4f;
import fi.dy.masa.malilib.util.EntityUtils;

public class RenderUtils
{
    public static void renderWallsWithLines(
            BlockPos posStart,
            BlockPos posEnd,
            Vec3d cameraPos,
            double lineIntervalH,
            double lineIntervalV,
            boolean alignLinesToModulo,
            Color4f color,
            BufferBuilder bufferQuads, BufferBuilder bufferLines)
    {
        Entity entity = EntityUtils.getCameraEntity();
        final int boxMinX = Math.min(posStart.getX(), posEnd.getX());
        final int boxMinZ = Math.min(posStart.getZ(), posEnd.getZ());
        final int boxMaxX = Math.max(posStart.getX(), posEnd.getX());
        final int boxMaxZ = Math.max(posStart.getZ(), posEnd.getZ());

        final int centerX = (int) Math.floor(entity.getX());
        final int centerZ = (int) Math.floor(entity.getZ());
        final int maxDist = MinecraftClient.getInstance().options.viewDistance * 32; // double the view distance in blocks
        final int rangeMinX = centerX - maxDist;
        final int rangeMinZ = centerZ - maxDist;
        final int rangeMaxX = centerX + maxDist;
        final int rangeMaxZ = centerZ + maxDist;
        final double minY = Math.min(posStart.getY(), posEnd.getY());
        final double maxY = Math.max(posStart.getY(), posEnd.getY()) + 1;
        double minX, minZ, maxX, maxZ;

        // The sides of the box along the x-axis can be at least partially inside the range
        if (rangeMinX <= boxMaxX && rangeMaxX >= boxMinX)
        {
            minX = Math.max(boxMinX, rangeMinX);
            maxX = Math.min(boxMaxX, rangeMaxX) + 1;

            if (rangeMinZ <= boxMinZ && rangeMaxZ >= boxMinZ)
            {
                minZ = maxZ = boxMinZ;
                renderWallWithLines(minX, minY, minZ, maxX, maxY, maxZ, lineIntervalH, lineIntervalV, alignLinesToModulo, cameraPos, color, bufferQuads, bufferLines);
            }

            if (rangeMinZ <= boxMaxZ && rangeMaxZ >= boxMaxZ)
            {
                minZ = maxZ = boxMaxZ + 1;
                renderWallWithLines(minX, minY, minZ, maxX, maxY, maxZ, lineIntervalH, lineIntervalV, alignLinesToModulo, cameraPos, color, bufferQuads, bufferLines);
            }
        }

        // The sides of the box along the z-axis can be at least partially inside the range
        if (rangeMinZ <= boxMaxZ && rangeMaxZ >= boxMinZ)
        {
            minZ = Math.max(boxMinZ, rangeMinZ);
            maxZ = Math.min(boxMaxZ, rangeMaxZ) + 1;

            if (rangeMinX <= boxMinX && rangeMaxX >= boxMinX)
            {
                minX = maxX = boxMinX;
                renderWallWithLines(minX, minY, minZ, maxX, maxY, maxZ, lineIntervalH, lineIntervalV, alignLinesToModulo, cameraPos, color, bufferQuads, bufferLines);
            }

            if (rangeMinX <= boxMaxX && rangeMaxX >= boxMaxX)
            {
                minX = maxX = boxMaxX + 1;
                renderWallWithLines(minX, minY, minZ, maxX, maxY, maxZ, lineIntervalH, lineIntervalV, alignLinesToModulo, cameraPos, color, bufferQuads, bufferLines);
            }
        }
    }

    public static void renderWallWithLines(
            double minX, double minY, double minZ,
            double maxX, double maxY, double maxZ,
            double lineIntervalH, double lineIntervalV,
            boolean alignLinesToModulo,
            Vec3d cameraPos,
            Color4f color,
            BufferBuilder bufferQuads, BufferBuilder bufferLines)
    {
        double cx = cameraPos.x;
        double cy = cameraPos.y;
        double cz = cameraPos.z;

        bufferQuads.vertex(minX - cx, maxY - cy, minZ - cz).color(color.r, color.g, color.b, color.a).next();
        bufferQuads.vertex(minX - cx, minY - cy, minZ - cz).color(color.r, color.g, color.b, color.a).next();
        bufferQuads.vertex(maxX - cx, minY - cy, maxZ - cz).color(color.r, color.g, color.b, color.a).next();
        bufferQuads.vertex(maxX - cx, maxY - cy, maxZ - cz).color(color.r, color.g, color.b, color.a).next();

        if (lineIntervalV > 0.0)
        {
            double lineY = alignLinesToModulo ? roundUp(minY, lineIntervalV) : minY;

            while (lineY <= maxY)
            {
                bufferLines.vertex(minX - cx, lineY - cy, minZ - cz).color(color.r, color.g, color.b, 1.0F).next();
                bufferLines.vertex(maxX - cx, lineY - cy, maxZ - cz).color(color.r, color.g, color.b, 1.0F).next();
                lineY += lineIntervalV;
            }
        }

        if (lineIntervalH > 0.0)
        {
            if (minX == maxX)
            {
                double lineZ = alignLinesToModulo ? roundUp(minZ, lineIntervalH) : minZ;

                while (lineZ <= maxZ)
                {
                    bufferLines.vertex(minX - cx, minY - cy, lineZ - cz).color(color.r, color.g, color.b, 1.0F).next();
                    bufferLines.vertex(minX - cx, maxY - cy, lineZ - cz).color(color.r, color.g, color.b, 1.0F).next();
                    lineZ += lineIntervalH;
                }
            }
            else if (minZ == maxZ)
            {
                double lineX = alignLinesToModulo ? roundUp(minX, lineIntervalH) : minX;

                while (lineX <= maxX)
                {
                    bufferLines.vertex(lineX - cx, minY - cy, minZ - cz).color(color.r, color.g, color.b, 1.0F).next();
                    bufferLines.vertex(lineX - cx, maxY - cy, minZ - cz).color(color.r, color.g, color.b, 1.0F).next();
                    lineX += lineIntervalH;
                }
            }
        }
    }

    public static double roundUp(double value, double interval)
    {
        if (interval == 0.0)
        {
            return 0.0;
        }
        else if (value == 0.0)
        {
            return interval;
        }
        else
        {
            if (value < 0.0)
            {
                interval *= -1.0;
            }

            double remainder = value % interval;

            return remainder == 0.0 ? value : value + interval - remainder;
        }
    }
}
