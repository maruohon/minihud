package fi.dy.masa.minihud.renderer;

import net.minecraft.client.renderer.BufferBuilder;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Vec3d;
import fi.dy.masa.malilib.util.GameUtils;
import fi.dy.masa.malilib.util.MathUtils;
import fi.dy.masa.malilib.util.data.Color4f;
import fi.dy.masa.malilib.util.wrap.EntityWrap;

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
        Entity entity = GameUtils.getCameraEntity();
        final int boxMinX = Math.min(posStart.getX(), posEnd.getX());
        final int boxMinZ = Math.min(posStart.getZ(), posEnd.getZ());
        final int boxMaxX = Math.max(posStart.getX(), posEnd.getX());
        final int boxMaxZ = Math.max(posStart.getZ(), posEnd.getZ());

        final int centerX = (int) Math.floor(EntityWrap.getX(entity));
        final int centerZ = (int) Math.floor(EntityWrap.getZ(entity));
        final int maxDist = GameUtils.getRenderDistanceChunks() * 16 * 2; // double the view distance in blocks
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

        bufferQuads.pos(minX - cx, maxY - cy, minZ - cz).color(color.r, color.g, color.b, color.a).endVertex();
        bufferQuads.pos(minX - cx, minY - cy, minZ - cz).color(color.r, color.g, color.b, color.a).endVertex();
        bufferQuads.pos(maxX - cx, minY - cy, maxZ - cz).color(color.r, color.g, color.b, color.a).endVertex();
        bufferQuads.pos(maxX - cx, maxY - cy, maxZ - cz).color(color.r, color.g, color.b, color.a).endVertex();

        if (lineIntervalV > 0.0)
        {
            double lineY = alignLinesToModulo ? MathUtils.roundUp(minY, lineIntervalV) : minY;

            while (lineY <= maxY)
            {
                bufferLines.pos(minX - cx, lineY - cy, minZ - cz).color(color.r, color.g, color.b, 1.0F).endVertex();
                bufferLines.pos(maxX - cx, lineY - cy, maxZ - cz).color(color.r, color.g, color.b, 1.0F).endVertex();
                lineY += lineIntervalV;
            }
        }

        if (lineIntervalH > 0.0)
        {
            if (minX == maxX)
            {
                double lineZ = alignLinesToModulo ? MathUtils.roundUp(minZ, lineIntervalH) : minZ;

                while (lineZ <= maxZ)
                {
                    bufferLines.pos(minX - cx, minY - cy, lineZ - cz).color(color.r, color.g, color.b, 1.0F).endVertex();
                    bufferLines.pos(minX - cx, maxY - cy, lineZ - cz).color(color.r, color.g, color.b, 1.0F).endVertex();
                    lineZ += lineIntervalH;
                }
            }
            else if (minZ == maxZ)
            {
                double lineX = alignLinesToModulo ? MathUtils.roundUp(minX, lineIntervalH) : minX;

                while (lineX <= maxX)
                {
                    bufferLines.pos(lineX - cx, minY - cy, minZ - cz).color(color.r, color.g, color.b, 1.0F).endVertex();
                    bufferLines.pos(lineX - cx, maxY - cy, minZ - cz).color(color.r, color.g, color.b, 1.0F).endVertex();
                    lineX += lineIntervalH;
                }
            }
        }
    }
}
