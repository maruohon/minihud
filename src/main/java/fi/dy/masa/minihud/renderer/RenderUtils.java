package fi.dy.masa.minihud.renderer;

import net.minecraft.client.render.BufferBuilder;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Direction;

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
        double posX = entity.x;
        double posZ = entity.z;
        float a = ((color >>> 24) & 0xFF) / 255f;
        float r = ((color >>> 16) & 0xFF) / 255f;
        float g = ((color >>>  8) & 0xFF) / 255f;
        float b = ((color       ) & 0xFF) / 255f;

        //double yMin = Math.max(Math.ceil((posY - rangeV) / lineIntervalV) * lineIntervalV,   0);
        //double yMax = Math.min(Math.ceil((posY + rangeV) / lineIntervalV) * lineIntervalV, 256);
        double yMin = posStart.getY();
        double yMax = posEnd.getY();

        renderVerticalWallsOfLinesIfWithinRange(bufferQuads, bufferLines, Direction.Axis.X, zMin, posZ, posX, yMin, yMax, rangeH,
                lineIntervalH, lineIntervalV, xMin, xMax, r, g, b, a);
        renderVerticalWallsOfLinesIfWithinRange(bufferQuads, bufferLines, Direction.Axis.X, zMax, posZ, posX, yMin, yMax, rangeH,
                lineIntervalH, lineIntervalV, xMin, xMax, r, g, b, a);

        renderVerticalWallsOfLinesIfWithinRange(bufferQuads, bufferLines, Direction.Axis.Z, xMin, posX, posZ, yMin, yMax, rangeH,
                lineIntervalH, lineIntervalV, zMin, zMax, r, g, b, a);
        renderVerticalWallsOfLinesIfWithinRange(bufferQuads, bufferLines, Direction.Axis.Z, xMax, posX, posZ, yMin, yMax, rangeH,
                lineIntervalH, lineIntervalV, zMin, zMax, r, g, b, a);
    }

    public static void renderVerticalWallsOfLinesWithinRange(
            BufferBuilder bufferQuads, BufferBuilder bufferLines,
            Direction.Axis axis,
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
        double posX = entity.x;
        double posZ = entity.z;
        float a = ((color >>> 24) & 0xFF) / 255f;
        float r = ((color >>> 16) & 0xFF) / 255f;
        float g = ((color >>>  8) & 0xFF) / 255f;
        float b = ((color       ) & 0xFF) / 255f;

        //double yMin = Math.max(Math.ceil((posY - rangeV) / lineIntervalV) * lineIntervalV,   0);
        //double yMax = Math.min(Math.ceil((posY + rangeV) / lineIntervalV) * lineIntervalV, 256);
        double yMin = posStart.getY();
        double yMax = posEnd.getY();

        if (axis == Direction.Axis.Z)
        {
            renderVerticalWallsOfLinesIfWithinRange(bufferQuads, bufferLines, Direction.Axis.X, zMin, posZ, posX, yMin, yMax, rangeH,
                    lineIntervalH, lineIntervalV, xMin, xMax, r, g, b, a);
        }
        else if (axis == Direction.Axis.X)
        {
            renderVerticalWallsOfLinesIfWithinRange(bufferQuads, bufferLines, Direction.Axis.Z, xMin, posX, posZ, yMin, yMax, rangeH,
                    lineIntervalH, lineIntervalV, zMin, zMax, r, g, b, a);
        }
    }

    public static void renderVerticalWallsOfLinesIfWithinRange(
            BufferBuilder bufferQuads, BufferBuilder bufferLines,
            Direction.Axis axis,
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
                        bufferLines.vertex(hMin, y, edge).method_22915(r, g, b, a).next();
                        bufferLines.vertex(hMax, y, edge).method_22915(r, g, b, a).next();
                    }

                    for (double h = hMin; h <= hMax; h += lineIntervalH)
                    {
                        bufferLines.vertex(h, yMin, edge).method_22915(r, g, b, a).next();
                        bufferLines.vertex(h, yMax, edge).method_22915(r, g, b, a).next();
                    }

                    bufferQuads.vertex(hMin, yMin, edge).method_22915(r, g, b, quadAlpha).next();
                    bufferQuads.vertex(hMin, yMax, edge).method_22915(r, g, b, quadAlpha).next();
                    bufferQuads.vertex(hMax, yMax, edge).method_22915(r, g, b, quadAlpha).next();
                    bufferQuads.vertex(hMax, yMin, edge).method_22915(r, g, b, quadAlpha).next();

                    break;
                case Z:
                    for (double y = yMin; y <= yMax; y += lineIntervalV)
                    {
                        bufferLines.vertex(edge, y, hMin).method_22915(r, g, b, a).next();
                        bufferLines.vertex(edge, y, hMax).method_22915(r, g, b, a).next();
                    }

                    for (double h = hMin; h <= hMax; h += lineIntervalH)
                    {
                        bufferLines.vertex(edge, yMin, h).method_22915(r, g, b, a).next();
                        bufferLines.vertex(edge, yMax, h).method_22915(r, g, b, a).next();
                    }

                    bufferQuads.vertex(edge, yMin, hMin).method_22915(r, g, b, quadAlpha).next();
                    bufferQuads.vertex(edge, yMax, hMin).method_22915(r, g, b, quadAlpha).next();
                    bufferQuads.vertex(edge, yMax, hMax).method_22915(r, g, b, quadAlpha).next();
                    bufferQuads.vertex(edge, yMin, hMax).method_22915(r, g, b, quadAlpha).next();
                    break;
                default:
            }
        }
    }
}
