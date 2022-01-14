package fi.dy.masa.minihud.renderer;

import java.util.Collection;
import net.minecraft.client.MinecraftClient;
import net.minecraft.client.render.BufferBuilder;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Direction;
import net.minecraft.util.math.Vec3d;
import net.minecraft.util.math.Vec3i;
import fi.dy.masa.malilib.util.Color4f;
import fi.dy.masa.malilib.util.EntityUtils;
import fi.dy.masa.malilib.util.LayerRange;
import fi.dy.masa.malilib.util.PositionUtils;
import fi.dy.masa.minihud.renderer.shapes.SideQuad;
import fi.dy.masa.minihud.util.ShapeRenderType;
import fi.dy.masa.minihud.util.shape.SphereUtils;
import it.unimi.dsi.fastutil.longs.LongOpenHashSet;

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


    /**
     * Assumes a BufferBuilder in GL_QUADS mode has been initialized
     */
    public static void drawBlockSpaceSideBatchedQuads(long posLong, Direction side,
                                                      Color4f color, double expand,
                                                      Vec3d cameraPos, BufferBuilder buffer)
    {
        int x = BlockPos.unpackLongX(posLong);
        int y = BlockPos.unpackLongY(posLong);
        int z = BlockPos.unpackLongZ(posLong);
        double offsetX = x - cameraPos.x;
        double offsetY = y - cameraPos.y;
        double offsetZ = z - cameraPos.z;
        double minX = offsetX - expand;
        double minY = offsetY - expand;
        double minZ = offsetZ - expand;
        double maxX = offsetX + expand + 1;
        double maxY = offsetY + expand + 1;
        double maxZ = offsetZ + expand + 1;

        switch (side)
        {
            case DOWN:
                buffer.vertex(maxX, minY, maxZ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(minX, minY, maxZ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(minX, minY, minZ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(maxX, minY, minZ).color(color.r, color.g, color.b, color.a).next();
                break;

            case UP:
                buffer.vertex(minX, maxY, maxZ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(maxX, maxY, maxZ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(maxX, maxY, minZ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(minX, maxY, minZ).color(color.r, color.g, color.b, color.a).next();
                break;

            case NORTH:
                buffer.vertex(maxX, minY, minZ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(minX, minY, minZ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(minX, maxY, minZ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(maxX, maxY, minZ).color(color.r, color.g, color.b, color.a).next();
                break;

            case SOUTH:
                buffer.vertex(minX, minY, maxZ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(maxX, minY, maxZ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(maxX, maxY, maxZ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(minX, maxY, maxZ).color(color.r, color.g, color.b, color.a).next();
                break;

            case WEST:
                buffer.vertex(minX, minY, minZ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(minX, minY, maxZ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(minX, maxY, maxZ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(minX, maxY, minZ).color(color.r, color.g, color.b, color.a).next();
                break;

            case EAST:
                buffer.vertex(maxX, minY, maxZ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(maxX, minY, minZ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(maxX, maxY, minZ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(maxX, maxY, maxZ).color(color.r, color.g, color.b, color.a).next();
                break;
        }
    }

    public static void renderCircleBlockPositions(LongOpenHashSet positions,
                                                  Direction[] sides,
                                                  SphereUtils.RingPositionTest test,
                                                  ShapeRenderType renderType,
                                                  LayerRange range,
                                                  Color4f color,
                                                  double expand,
                                                  Vec3d cameraPos,
                                                  BufferBuilder buffer)
    {
        boolean full = renderType == ShapeRenderType.FULL_BLOCK;
        boolean outer = renderType == ShapeRenderType.OUTER_EDGE;
        boolean inner = renderType == ShapeRenderType.INNER_EDGE;
        //int count = 0;

        for (long posLong : positions)
        {
            if (range.isPositionWithinRange(posLong) == false)
            {
                continue;
            }

            for (Direction side : sides)
            {
                long adjPosLong = BlockPos.offset(posLong, side);

                if (positions.contains(adjPosLong))
                {
                    continue;
                }

                boolean render = full;

                if (full == false)
                {
                    int adjX = BlockPos.unpackLongX(adjPosLong);
                    int adjY = BlockPos.unpackLongY(adjPosLong);
                    int adjZ = BlockPos.unpackLongZ(adjPosLong);
                    boolean onOrIn = test.isInsideOrCloserThan(adjX, adjY, adjZ, side);
                    render = ((outer && onOrIn == false) || (inner && onOrIn));
                }

                if (render)
                {
                    RenderUtils.drawBlockSpaceSideBatchedQuads(posLong, side, color, expand, cameraPos, buffer);
                    //++count;
                }
            }
        }
        //System.out.printf("individual: rendered %d quads\n", count);
    }


    public static void renderBlockPositions(LongOpenHashSet positions,
                                            LayerRange range,
                                            Color4f color,
                                            double expand,
                                            Vec3d cameraPos,
                                            BufferBuilder buffer)
    {
        //int count = 0;
        for (long posLong : positions)
        {
            if (range.isPositionWithinRange(posLong) == false)
            {
                continue;
            }

            for (Direction side : PositionUtils.ALL_DIRECTIONS)
            {
                long adjPosLong = BlockPos.offset(posLong, side);

                if (positions.contains(adjPosLong))
                {
                    continue;
                }

                RenderUtils.drawBlockSpaceSideBatchedQuads(posLong, side, color, expand, cameraPos, buffer);
                //++count;
            }
        }
        //System.out.printf("individual: rendered %d quads\n", count);
    }

    public static void renderQuads(Collection<SideQuad> quads, Color4f color, double expand,
                                   Vec3d cameraPos, BufferBuilder buffer)
    {
        for (SideQuad quad : quads)
        {
            RenderUtils.renderInsetQuad(quad.startPos(), quad.width(), quad.height(), quad.side(),
                                        -expand, color, cameraPos, buffer);
        }
        //System.out.printf("merged: rendered %d quads\n", quads.size());
    }

    public static void renderInsetQuad(Vec3i minPos, int width, int height, Direction side,
                                       double inset, Color4f color, Vec3d cameraPos, BufferBuilder buffer)
    {
        renderInsetQuad(minPos.getX(), minPos.getY(), minPos.getZ(), width, height, side, inset, color, cameraPos, buffer);
    }

    public static void renderInsetQuad(long minPos, int width, int height, Direction side,
                                       double inset, Color4f color, Vec3d cameraPos, BufferBuilder buffer)
    {
        int x = BlockPos.unpackLongX(minPos);
        int y = BlockPos.unpackLongY(minPos);
        int z = BlockPos.unpackLongZ(minPos);

        renderInsetQuad(x, y, z, width, height, side, inset, color, cameraPos, buffer);
    }

    public static void renderInsetQuad(int x, int y, int z, int width, int height, Direction side,
                                       double inset, Color4f color, Vec3d cameraPos, BufferBuilder buffer)
    {
        double minX = x - cameraPos.x;
        double minY = y - cameraPos.y;
        double minZ = z - cameraPos.z;
        double maxX = minX;
        double maxY = minY;
        double maxZ = minZ;

        if (side.getAxis() == Direction.Axis.Z)
        {
            maxX += width;
            maxY += height;
        }
        else if (side.getAxis() == Direction.Axis.X)
        {
            maxY += height;
            maxZ += width;
        }
        else if (side.getAxis() == Direction.Axis.Y)
        {
            maxX += width;
            maxZ += height;
        }

        switch (side)
        {
            case WEST:
                minX += inset;
                buffer.vertex(minX, minY, minZ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(minX, maxY, minZ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(minX, maxY, maxZ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(minX, minY, maxZ).color(color.r, color.g, color.b, color.a).next();
                break;
            case EAST:
                maxX += 1 - inset;
                buffer.vertex(maxX, minY, minZ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(maxX, minY, maxZ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(maxX, maxY, maxZ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(maxX, maxY, minZ).color(color.r, color.g, color.b, color.a).next();
                break;

            case NORTH:
                minZ += inset;
                buffer.vertex(minX, minY, minZ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(maxX, minY, minZ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(maxX, maxY, minZ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(minX, maxY, minZ).color(color.r, color.g, color.b, color.a).next();
                break;

            case SOUTH:
                maxZ += 1 - inset;
                buffer.vertex(minX, minY, maxZ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(minX, maxY, maxZ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(maxX, maxY, maxZ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(maxX, minY, maxZ).color(color.r, color.g, color.b, color.a).next();
                break;

            case DOWN:
                minY += inset;
                buffer.vertex(minX, minY, minZ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(minX, minY, maxZ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(maxX, minY, maxZ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(maxX, minY, minZ).color(color.r, color.g, color.b, color.a).next();
                break;

            case UP:
                maxY += 1 - inset;
                buffer.vertex(minX, maxY, minZ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(maxX, maxY, minZ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(maxX, maxY, maxZ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(minX, maxY, maxZ).color(color.r, color.g, color.b, color.a).next();
                break;
        }
    }

    public static void renderBiomeBorderLines(Vec3i minPos,
                                              int width,
                                              int height,
                                              Direction side,
                                              double inset,
                                              Color4f color,
                                              Vec3d cameraPos,
                                              BufferBuilder buffer)
    {
        double minX = minPos.getX() - cameraPos.x;
        double minY = minPos.getY() - cameraPos.y;
        double minZ = minPos.getZ() - cameraPos.z;

        switch (side)
        {
            case WEST   -> minX += inset;
            case EAST   -> minX += 1 - inset;
            case NORTH  -> minZ += inset;
            case SOUTH  -> minZ += 1 - inset;
            case DOWN   -> minY += inset;
            case UP     -> minY += 1 - inset;
        }

        double maxX = minX;
        double maxY = minY;
        double maxZ = minZ;

        if (side.getAxis() == Direction.Axis.Z)
        {
            maxX += width;
            maxY += height;
        }
        else if (side.getAxis() == Direction.Axis.X)
        {
            maxY += height;
            maxZ += width;
        }
        else if (side.getAxis() == Direction.Axis.Y)
        {
            maxX += width;
            maxZ += height;
        }

        if (side.getAxis() == Direction.Axis.Y)
        {
            // Line at the "start" end of the quad
            buffer.vertex(minX, minY, minZ).color(color.r, color.g, color.b, 1f).next();
            buffer.vertex(minX, maxY, maxZ).color(color.r, color.g, color.b, 1f).next();

            for (double z = minZ; z < maxZ + 0.5; z += 1.0)
            {
                buffer.vertex(minX, minY, z).color(color.r, color.g, color.b, 1f).next();
                buffer.vertex(maxX, maxY, z).color(color.r, color.g, color.b, 1f).next();
            }
        }
        else
        {
            // Vertical line at the "start" end of the quad
            buffer.vertex(minX, minY, minZ).color(color.r, color.g, color.b, 1f).next();
            buffer.vertex(minX, maxY, minZ).color(color.r, color.g, color.b, 1f).next();

            for (double y = minY; y < maxY + 0.5; y += 1.0)
            {
                buffer.vertex(minX, y, minZ).color(color.r, color.g, color.b, 1f).next();
                buffer.vertex(maxX, y, maxZ).color(color.r, color.g, color.b, 1f).next();
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
