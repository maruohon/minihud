package fi.dy.masa.minihud.renderer;

import net.minecraft.client.renderer.BufferBuilder;
import net.minecraft.entity.Entity;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;

public class RenderUtils
{
    public static void renderVerticalWallsOfLinesWithinRange(
            BufferBuilder bufferQuads, BufferBuilder bufferLines,
            BlockPos posStart,
            BlockPos posEnd,
            float rangeH,
            float rangeV,
            float lineIntervalH,
            float lineIntervalV,
            Entity entity,
            int color)
    {
        int xMin = Math.min(posStart.getX(), posEnd.getX());
        int zMin = Math.min(posStart.getZ(), posEnd.getZ());
        int xMax = Math.max(posStart.getX(), posEnd.getX()) + 1;
        int zMax = Math.max(posStart.getZ(), posEnd.getZ()) + 1;
        double posX = entity.posX;
        double posZ = entity.posZ;
        float a = ((color >>> 24) & 0xFF) / 255f;
        float r = ((color >>> 16) & 0xFF) / 255f;
        float g = ((color >>>  8) & 0xFF) / 255f;
        float b = ((color       ) & 0xFF) / 255f;

        //double yMin = Math.max(Math.ceil((posY - rangeV) / lineIntervalV) * lineIntervalV,   0);
        //double yMax = Math.min(Math.ceil((posY + rangeV) / lineIntervalV) * lineIntervalV, 256);
        double yMin = posStart.getY();
        double yMax = posEnd.getY();

        renderVerticalWallsOfLinesIfWithinRange(bufferQuads, bufferLines, EnumFacing.Axis.X, zMin, posZ, posX, yMin, yMax, rangeH,
                lineIntervalH, lineIntervalV, xMin, xMax, r, g, b, a);
        renderVerticalWallsOfLinesIfWithinRange(bufferQuads, bufferLines, EnumFacing.Axis.X, zMax, posZ, posX, yMin, yMax, rangeH,
                lineIntervalH, lineIntervalV, xMin, xMax, r, g, b, a);

        renderVerticalWallsOfLinesIfWithinRange(bufferQuads, bufferLines, EnumFacing.Axis.Z, xMin, posX, posZ, yMin, yMax, rangeH,
                lineIntervalH, lineIntervalV, zMin, zMax, r, g, b, a);
        renderVerticalWallsOfLinesIfWithinRange(bufferQuads, bufferLines, EnumFacing.Axis.Z, xMax, posX, posZ, yMin, yMax, rangeH,
                lineIntervalH, lineIntervalV, zMin, zMax, r, g, b, a);
    }

    public static void renderVerticalWallsOfLinesWithinRange(
            BufferBuilder bufferQuads, BufferBuilder bufferLines,
            EnumFacing.Axis axis,
            BlockPos posStart,
            BlockPos posEnd,
            float rangeH,
            float rangeV,
            float lineIntervalH,
            float lineIntervalV,
            Entity entity,
            int color)
    {
        int xMin = Math.min(posStart.getX(), posEnd.getX());
        int zMin = Math.min(posStart.getZ(), posEnd.getZ());
        int xMax = Math.max(posStart.getX(), posEnd.getX());
        int zMax = Math.max(posStart.getZ(), posEnd.getZ());
        double posX = entity.posX;
        double posZ = entity.posZ;
        float a = ((color >>> 24) & 0xFF) / 255f;
        float r = ((color >>> 16) & 0xFF) / 255f;
        float g = ((color >>>  8) & 0xFF) / 255f;
        float b = ((color       ) & 0xFF) / 255f;

        //double yMin = Math.max(Math.ceil((posY - rangeV) / lineIntervalV) * lineIntervalV,   0);
        //double yMax = Math.min(Math.ceil((posY + rangeV) / lineIntervalV) * lineIntervalV, 256);
        double yMin = posStart.getY();
        double yMax = posEnd.getY();

        if (axis == EnumFacing.Axis.Z)
        {
            renderVerticalWallsOfLinesIfWithinRange(bufferQuads, bufferLines, EnumFacing.Axis.X, zMin, posZ, posX, yMin, yMax, rangeH,
                    lineIntervalH, lineIntervalV, xMin, xMax, r, g, b, a);
        }
        else if (axis == EnumFacing.Axis.X)
        {
            renderVerticalWallsOfLinesIfWithinRange(bufferQuads, bufferLines, EnumFacing.Axis.Z, xMin, posX, posZ, yMin, yMax, rangeH,
                    lineIntervalH, lineIntervalV, zMin, zMax, r, g, b, a);
        }
    }

    public static void renderVerticalWallsOfLinesIfWithinRange(
            BufferBuilder bufferQuads, BufferBuilder bufferLines,
            EnumFacing.Axis axis,
            double edge, double posOnEdgeAxis, double posOnPerpAxis,
            double yMin, double yMax, float rangeH,
            float lineIntervalH, float lineIntervalV,
            int perpendicularMin, int perpendicularMax,
            float r, float g, float b, float a)
    {
        if (Math.abs(posOnEdgeAxis - edge) <= rangeH)
        {
            double hMin = Math.max(Math.ceil((posOnPerpAxis - rangeH) / lineIntervalH) * lineIntervalV, perpendicularMin);
            double hMax = Math.min(Math.ceil((posOnPerpAxis + rangeH) / lineIntervalH) * lineIntervalV, perpendicularMax);
            float quadAlpha = a / 6f;

            switch (axis)
            {
                case X:
                    for (double y = yMin; y <= yMax; y += lineIntervalV)
                    {
                        bufferLines.pos(hMin, y, edge).color(r, g, b, a).endVertex();
                        bufferLines.pos(hMax, y, edge).color(r, g, b, a).endVertex();
                    }

                    for (double h = hMin; h <= hMax; h += lineIntervalH)
                    {
                        bufferLines.pos(h, yMin, edge).color(r, g, b, a).endVertex();
                        bufferLines.pos(h, yMax, edge).color(r, g, b, a).endVertex();
                    }

                    bufferQuads.pos(hMin, yMin, edge).color(r, g, b, quadAlpha).endVertex();
                    bufferQuads.pos(hMin, yMax, edge).color(r, g, b, quadAlpha).endVertex();
                    bufferQuads.pos(hMax, yMax, edge).color(r, g, b, quadAlpha).endVertex();
                    bufferQuads.pos(hMax, yMin, edge).color(r, g, b, quadAlpha).endVertex();

                    break;
                case Z:
                    for (double y = yMin; y <= yMax; y += lineIntervalV)
                    {
                        bufferLines.pos(edge, y, hMin).color(r, g, b, a).endVertex();
                        bufferLines.pos(edge, y, hMax).color(r, g, b, a).endVertex();
                    }

                    for (double h = hMin; h <= hMax; h += lineIntervalH)
                    {
                        bufferLines.pos(edge, yMin, h).color(r, g, b, a).endVertex();
                        bufferLines.pos(edge, yMax, h).color(r, g, b, a).endVertex();
                    }

                    bufferQuads.pos(edge, yMin, hMin).color(r, g, b, quadAlpha).endVertex();
                    bufferQuads.pos(edge, yMax, hMin).color(r, g, b, quadAlpha).endVertex();
                    bufferQuads.pos(edge, yMax, hMax).color(r, g, b, quadAlpha).endVertex();
                    bufferQuads.pos(edge, yMin, hMax).color(r, g, b, quadAlpha).endVertex();
                    break;
                default:
            }
        }
    }
}
