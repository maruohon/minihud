package minihud.renderer;

import net.minecraft.entity.Entity;

import malilib.render.buffer.VertexBuilder;
import malilib.util.MathUtils;
import malilib.util.data.Color4f;
import malilib.util.game.wrap.EntityWrap;
import malilib.util.game.wrap.GameWrap;
import malilib.util.position.BlockPos;
import malilib.util.position.Vec3d;

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
            VertexBuilder quadBuilder, VertexBuilder lineBuilder)
    {
        Entity entity = GameWrap.getCameraEntity();
        final int boxMinX = Math.min(posStart.getX(), posEnd.getX());
        final int boxMinZ = Math.min(posStart.getZ(), posEnd.getZ());
        final int boxMaxX = Math.max(posStart.getX(), posEnd.getX());
        final int boxMaxZ = Math.max(posStart.getZ(), posEnd.getZ());

        final int centerX = (int) Math.floor(EntityWrap.getX(entity));
        final int centerZ = (int) Math.floor(EntityWrap.getZ(entity));
        final int maxDist = GameWrap.getRenderDistanceChunks() * 16 * 2; // double the view distance in blocks
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
                renderWallWithLines(minX, minY, minZ, maxX, maxY, maxZ, lineIntervalH, lineIntervalV, alignLinesToModulo, cameraPos, color, quadBuilder, lineBuilder);
            }

            if (rangeMinZ <= boxMaxZ && rangeMaxZ >= boxMaxZ)
            {
                minZ = maxZ = boxMaxZ + 1;
                renderWallWithLines(minX, minY, minZ, maxX, maxY, maxZ, lineIntervalH, lineIntervalV, alignLinesToModulo, cameraPos, color, quadBuilder, lineBuilder);
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
                renderWallWithLines(minX, minY, minZ, maxX, maxY, maxZ, lineIntervalH, lineIntervalV, alignLinesToModulo, cameraPos, color, quadBuilder, lineBuilder);
            }

            if (rangeMinX <= boxMaxX && rangeMaxX >= boxMaxX)
            {
                minX = maxX = boxMaxX + 1;
                renderWallWithLines(minX, minY, minZ, maxX, maxY, maxZ, lineIntervalH, lineIntervalV, alignLinesToModulo, cameraPos, color, quadBuilder, lineBuilder);
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
            VertexBuilder quadBuilder, VertexBuilder lineBuilder)
    {
        double cx = cameraPos.x;
        double cy = cameraPos.y;
        double cz = cameraPos.z;

        quadBuilder.posColor(minX - cx, maxY - cy, minZ - cz, color);
        quadBuilder.posColor(minX - cx, minY - cy, minZ - cz, color);
        quadBuilder.posColor(maxX - cx, minY - cy, maxZ - cz, color);
        quadBuilder.posColor(maxX - cx, maxY - cy, maxZ - cz, color);

        if (lineIntervalV > 0.0)
        {
            double lineY = alignLinesToModulo ? MathUtils.roundUp(minY, lineIntervalV) : minY;

            while (lineY <= maxY)
            {
                lineBuilder.posColor(minX - cx, lineY - cy, minZ - cz, color.ri, color.gi, color.bi, 255);
                lineBuilder.posColor(maxX - cx, lineY - cy, maxZ - cz, color.ri, color.gi, color.bi, 255);
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
                    lineBuilder.posColor(minX - cx, minY - cy, lineZ - cz, color.ri, color.gi, color.bi, 255);
                    lineBuilder.posColor(minX - cx, maxY - cy, lineZ - cz, color.ri, color.gi, color.bi, 255);
                    lineZ += lineIntervalH;
                }
            }
            else if (minZ == maxZ)
            {
                double lineX = alignLinesToModulo ? MathUtils.roundUp(minX, lineIntervalH) : minX;

                while (lineX <= maxX)
                {
                    lineBuilder.posColor(lineX - cx, minY - cy, minZ - cz, color.ri, color.gi, color.bi, 255);
                    lineBuilder.posColor(lineX - cx, maxY - cy, minZ - cz, color.ri, color.gi, color.bi, 255);
                    lineX += lineIntervalH;
                }
            }
        }
    }
}
