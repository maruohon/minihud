package fi.dy.masa.minihud.util.shape;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;
import java.util.function.Function;
import javax.annotation.Nullable;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Direction;
import net.minecraft.util.math.Vec3d;
import fi.dy.masa.malilib.util.LayerRange;
import fi.dy.masa.malilib.util.PositionUtils;
import fi.dy.masa.minihud.renderer.shapes.SideQuad;
import fi.dy.masa.minihud.util.ShapeRenderType;
import it.unimi.dsi.fastutil.longs.Long2ByteOpenHashMap;
import it.unimi.dsi.fastutil.longs.Long2ObjectOpenHashMap;
import it.unimi.dsi.fastutil.longs.LongOpenHashSet;

public class SphereUtils
{
    public static void collectSpherePositions(Consumer<BlockPos.Mutable> positionConsumer,
                                              SphereUtils.RingPositionTest test,
                                              BlockPos centerPos, int radius)
    {
        BlockPos.Mutable mutablePos = new BlockPos.Mutable();

        //long before = System.nanoTime();
        mutablePos.set(centerPos);
        SphereUtils.addPositionsOnHorizontalBlockRing(positionConsumer, mutablePos, test);

        mutablePos.set(centerPos);
        SphereUtils.addPositionsOnVerticalBlockRing(positionConsumer, mutablePos, Direction.NORTH, test);

        final int r = radius + 2;

        for (int i = 1; i < r; ++i)
        {
            // Horizontal rings
            mutablePos.set(centerPos.getX(), centerPos.getY() - i, centerPos.getZ());
            SphereUtils.addPositionsOnHorizontalBlockRing(positionConsumer, mutablePos, test);

            mutablePos.set(centerPos.getX(), centerPos.getY() + i, centerPos.getZ());
            SphereUtils.addPositionsOnHorizontalBlockRing(positionConsumer, mutablePos, test);

            // Vertical rings
            mutablePos.set(centerPos.getX(), centerPos.getY(), centerPos.getZ() - i);
            SphereUtils.addPositionsOnVerticalBlockRing(positionConsumer, mutablePos, Direction.NORTH, test);

            mutablePos.set(centerPos.getX(), centerPos.getY(), centerPos.getZ() + i);
            SphereUtils.addPositionsOnVerticalBlockRing(positionConsumer, mutablePos, Direction.NORTH, test);
        }
        //System.out.printf("time: %.6f s\n", (double) (System.nanoTime() - before) / 1000000000D);
    }

    public static boolean movePositionToRing(BlockPos.Mutable posMutable,
                                             Direction moveDirection,
                                             RingPositionTest test)
    {
        final int incX = moveDirection.getOffsetX();
        final int incY = moveDirection.getOffsetY();
        final int incZ = moveDirection.getOffsetZ();
        int x = posMutable.getX();
        int y = posMutable.getY();
        int z = posMutable.getZ();
        int nextX = x;
        int nextY = y;
        int nextZ = z;

        while (test.isInsideOrCloserThan(nextX, nextY, nextZ, moveDirection))
        {
            x = nextX;
            y = nextY;
            z = nextZ;
            nextX += incX;
            nextY += incY;
            nextZ += incZ;
        }

        // Successfully entered the loop at least once
        if (x != nextX || y != nextY | z != nextZ)
        {
            posMutable.set(x, y, z);
            return true;
        }

        return false;
    }

    public static void addPositionsOnHorizontalBlockRing(Consumer<BlockPos.Mutable> positionConsumer,
                                                         BlockPos.Mutable mutablePos,
                                                         RingPositionTest test)
    {
        Function<Direction, Direction> nextDirectionFunction = SphereUtils::getNextHorizontalDirection;
        Direction startDirection = Direction.EAST;
        addPositionsOnBlockRing(positionConsumer, mutablePos, startDirection, test, nextDirectionFunction);
    }

    public static void addPositionsOnVerticalBlockRing(Consumer<BlockPos.Mutable> positionConsumer,
                                                       BlockPos.Mutable mutablePos,
                                                       Direction mainAxis,
                                                       RingPositionTest test)
    {
        Function<Direction, Direction> nextDirectionFunction = (dir) -> SphereUtils.getNextVerticalRingDirection(dir, mainAxis);
        Direction startDirection = Direction.UP;
        addPositionsOnBlockRing(positionConsumer, mutablePos, startDirection, test, nextDirectionFunction);
    }

    public static void addPositionsOnBlockRing(Consumer<BlockPos.Mutable> positionConsumer,
                                               BlockPos.Mutable mutablePos,
                                               Direction startDirection,
                                               RingPositionTest test,
                                               Function<Direction, Direction> nextDirectionFunction)
    {
        if (movePositionToRing(mutablePos, startDirection, test))
        {
            LongOpenHashSet seenPositions = new LongOpenHashSet();
            final BlockPos firstPos = mutablePos.toImmutable();
            Direction direction = startDirection;

            positionConsumer.accept(mutablePos);

            while (true)
            {
                direction = getNextPositionOnBlockRing(mutablePos, direction, test, nextDirectionFunction);
                long posLong = mutablePos.asLong();

                if (direction == null || mutablePos.equals(firstPos) || seenPositions.contains(posLong))
                {
                    break;
                }

                positionConsumer.accept(mutablePos);
                seenPositions.add(posLong);
            }
        }
    }

    @Nullable
    public static Direction getNextPositionOnBlockRing(BlockPos.Mutable posMutable,
                                                       Direction escapeDirection,
                                                       RingPositionTest test,
                                                       Function<Direction, Direction> nextDirectionFunction)
    {
        Direction dirOut = escapeDirection;
        Direction ccw90;

        for (int i = 0; i < 4; ++i)
        {
            int x = posMutable.getX() + escapeDirection.getOffsetX();
            int y = posMutable.getY() + escapeDirection.getOffsetY();
            int z = posMutable.getZ() + escapeDirection.getOffsetZ();

            // First check the adjacent position
            if (test.isInsideOrCloserThan(x, y, z, escapeDirection))
            {
                posMutable.set(x, y, z);
                return dirOut;
            }

            ccw90 = nextDirectionFunction.apply(escapeDirection);

            // Then check the diagonal position
            x += ccw90.getOffsetX();
            y += ccw90.getOffsetY();
            z += ccw90.getOffsetZ();

            if (test.isInsideOrCloserThan(x, y, z, escapeDirection))
            {
                posMutable.set(x, y, z);
                return dirOut;
            }

            // Delay the next direction by one cycle, so that it won't get updated too soon on the diagonals
            dirOut = escapeDirection;
            escapeDirection = nextDirectionFunction.apply(escapeDirection);
        }

        return null;
    }

    public static boolean isPositionInsideOrClosestToRadiusOnBlockRing(int blockX,
                                                                       int blockY,
                                                                       int blockZ,
                                                                       Vec3d center,
                                                                       double squareRadius,
                                                                       Direction escapeDirection)
    {
        double x = (double) blockX + 0.5;
        double y = (double) blockY + 0.5;
        double z = (double) blockZ + 0.5;
        double dist = center.squaredDistanceTo(x, y, z);
        double diff = squareRadius - dist;

        if (diff > 0)
        {
            return true;
        }

        double xAdj = (double) blockX + escapeDirection.getOffsetX() + 0.5;
        double yAdj = (double) blockY + escapeDirection.getOffsetY() + 0.5;
        double zAdj = (double) blockZ + escapeDirection.getOffsetZ() + 0.5;
        double distAdj = center.squaredDistanceTo(xAdj, yAdj, zAdj);
        double diffAdj = squareRadius - distAdj;

        return diffAdj > 0 && Math.abs(diff) < Math.abs(diffAdj);
    }

    /**
     * Returns the next horizontal direction in sequence, rotating counter-clockwise
     */
    public static Direction getNextHorizontalDirection(Direction dirIn)
    {
        return dirIn.rotateYCounterclockwise();
    }

    /**
     * Returns the next direction in sequence, rotating up to north
     */
    public static Direction getNextVerticalRingDirection(Direction currentDirection, Direction mainAxis)
    {
        return switch (mainAxis)
               {
                   case UP, DOWN -> switch (currentDirection)
                                    {
                                        case NORTH -> Direction.DOWN;
                                        case SOUTH -> Direction.UP;
                                        case DOWN -> Direction.SOUTH;
                                        default -> Direction.NORTH;
                                    };
                   case NORTH, SOUTH -> switch (currentDirection)
                                        {
                                            case WEST -> Direction.UP;
                                            case EAST -> Direction.DOWN;
                                            case DOWN -> Direction.WEST;
                                            default -> Direction.EAST;
                                        };
                   case WEST, EAST -> switch (currentDirection)
                                      {
                                          case NORTH -> Direction.UP;
                                          case SOUTH -> Direction.DOWN;
                                          case DOWN -> Direction.NORTH;
                                          default -> Direction.SOUTH;
                                      };
               };

    }

    public static Direction[] getDirectionsNotOnAxis(Direction.Axis axis)
    {
        Direction[] sides = new Direction[4];
        int index = 0;

        for (Direction side : PositionUtils.ALL_DIRECTIONS)
        {
            // Exclude the two sides on the main axis
            if (side.getAxis() != axis)
            {
                sides[index++] = side;
            }
        }

        return sides;
    }

    public static List<SideQuad> buildSphereShellToQuads(LongOpenHashSet positions,
                                                         Direction.Axis mainAxis,
                                                         SphereUtils.RingPositionTest test,
                                                         ShapeRenderType renderType,
                                                         LayerRange layerRange)
    {
        Long2ObjectOpenHashMap<SideQuad> strips = buildSphereShellToStrips(positions, mainAxis,
                                                                           test, renderType, layerRange);
        return buildStripsToQuads(strips, mainAxis);
    }

    public static Long2ObjectOpenHashMap<SideQuad> buildSphereShellToStrips(LongOpenHashSet positions,
                                                                            Direction.Axis mainAxis,
                                                                            SphereUtils.RingPositionTest test,
                                                                            ShapeRenderType renderType,
                                                                            LayerRange layerRange)
    {
        Long2ObjectOpenHashMap<SideQuad> strips = new Long2ObjectOpenHashMap<>();
        Long2ByteOpenHashMap handledPositions = new Long2ByteOpenHashMap();
        Direction[] sides = PositionUtils.ALL_DIRECTIONS;

        for (long pos : positions)
        {
            if (layerRange.isPositionWithinRange(pos) == false)
            {
                continue;
            }

            for (Direction side : sides)
            {
                if (isHandledAndMarkHandled(pos, side, handledPositions) ||
                    shouldRenderSide(pos, side, test, renderType, positions) == false)
                {
                    continue;
                }

                final Direction minDir = side.getAxis() != mainAxis ? getNegativeDirectionFor(getThirdAxis(mainAxis, side.getAxis())) : (mainAxis.isVertical() ? Direction.WEST : Direction.DOWN);
                //final Direction minDir = getNegativeDirectionFor(getThirdAxis(mainAxis, side.getAxis()));
                final Direction maxDir = minDir.getOpposite();
                final int lengthMin = getStripLengthOnSide(pos, side, minDir, test, renderType, positions, handledPositions);
                final int lengthMax = getStripLengthOnSide(pos, side, maxDir, test, renderType, positions, handledPositions);
                final long startPosLong = offsetPos(pos, minDir, lengthMin);
                final int length = lengthMin + lengthMax + 1;
                final long index = getCompressedPosSide(startPosLong, side);

                strips.put(index, new SideQuad(startPosLong, length, 1, side));
            }
        }

        return strips;
    }

    public static List<SideQuad> buildStripsToQuads(Long2ObjectOpenHashMap<SideQuad> strips,
                                                    Direction.Axis mainAxis)
    {
        List<SideQuad> quads = new ArrayList<>();
        Long2ByteOpenHashMap handledPositions = new Long2ByteOpenHashMap();

        for (SideQuad strip : strips.values())
        {
            final long pos = strip.startPos();
            final Direction side = strip.side();

            if (isHandledAndMarkHandled(pos, side, handledPositions))
            {
                continue;
            }

            final Direction minDir = side.getAxis() != mainAxis ? getNegativeDirectionFor(mainAxis) : (mainAxis.isVertical() ? Direction.NORTH : Direction.DOWN);
            final Direction maxDir = minDir.getOpposite();
            final int stripCountMin = getStripCountOnSide(strip, minDir, strips, handledPositions);
            final int stripCountMax = getStripCountOnSide(strip, maxDir, strips, handledPositions);
            final long startPos = offsetPos(pos, minDir, stripCountMin);
            final int height = stripCountMin + stripCountMax + 1;

            quads.add(new SideQuad(startPos, strip.width(), height, side));
        }

        return quads;
    }

    protected static int getStripCountOnSide(SideQuad startStrip,
                                             Direction offsetSide,
                                             Long2ObjectOpenHashMap<SideQuad> strips,
                                             Long2ByteOpenHashMap handledPositions)
    {
        final long startPos = startStrip.startPos();
        final Direction side = startStrip.side();
        final int width = startStrip.width();
        long adjPos = BlockPos.offset(startPos, offsetSide);
        int count = 0;

        while (true)
        {
            long index = getCompressedPosSide(adjPos, side);
            SideQuad adjStrip = strips.get(index);

            // Note: the isHandled call needs to be the last call,
            // so that we don't mark things handled when they have not been handled!
            if (adjStrip == null || adjStrip.width() != width ||
                isHandledAndMarkHandled(adjPos, side, handledPositions))
            {
                break;
            }

            ++count;
            adjPos = BlockPos.offset(adjPos, offsetSide);
        }

        return count;
    }

    protected static int getStripLengthOnSide(long pos,
                                              Direction side,
                                              Direction moveDirection,
                                              SphereUtils.RingPositionTest test,
                                              ShapeRenderType renderType,
                                              LongOpenHashSet positions,
                                              Long2ByteOpenHashMap handledPositions)
    {
        int length = 0;
        long adjPos = BlockPos.offset(pos, moveDirection);

        while (positions.contains(adjPos))
        {
            if (shouldRenderSide(adjPos, side, test, renderType, positions) == false ||
                isHandledAndMarkHandled(adjPos, side, handledPositions))
            {
                break;
            }

            ++length;
            adjPos = BlockPos.offset(adjPos, moveDirection);
        }

        return length;
    }

    public static boolean isHandledAndMarkHandled(long pos,
                                                  Direction side,
                                                  Long2ByteOpenHashMap handledPositions)
    {
        byte sideMask = (byte) (1 << side.getId());
        byte val = handledPositions.get(pos);

        if ((val & sideMask) != 0)
        {
            return true;
        }

        val |= sideMask;
        handledPositions.put(pos, val);

        return false;
    }

    protected static boolean shouldRenderSide(long pos,
                                              Direction side,
                                              SphereUtils.RingPositionTest test,
                                              ShapeRenderType renderType,
                                              LongOpenHashSet positions)
    {
        long adjPos = BlockPos.offset(pos, side);

        if (positions.contains(adjPos))
        {
            return false;
        }

        if (renderType == ShapeRenderType.FULL_BLOCK)
        {
            return true;
        }

        int adjX = BlockPos.unpackLongX(adjPos);
        int adjY = BlockPos.unpackLongY(adjPos);
        int adjZ = BlockPos.unpackLongZ(adjPos);
        boolean onOrIn = test.isInsideOrCloserThan(adjX, adjY, adjZ, side);

        return ((renderType == ShapeRenderType.OUTER_EDGE && onOrIn == false) ||
                (renderType == ShapeRenderType.INNER_EDGE && onOrIn));
    }

    public static Direction getNegativeDirectionFor(Direction.Axis axis)
    {
        return switch (axis) {
            case X -> Direction.WEST;
            case Y -> Direction.DOWN;
            case Z -> Direction.NORTH;
        };
    }

    public static Direction getPositiveDirectionFor(Direction.Axis axis)
    {
        return switch (axis) {
            case X -> Direction.EAST;
            case Y -> Direction.UP;
            case Z -> Direction.SOUTH;
        };
    }

    public static Direction.Axis getThirdAxis(Direction.Axis axis1, Direction.Axis axis2)
    {
        return switch (axis1)
               {
                   case X -> axis2 == Direction.Axis.Y ? Direction.Axis.Z : Direction.Axis.Y;
                   case Y -> axis2 == Direction.Axis.X ? Direction.Axis.Z : Direction.Axis.X;
                   case Z -> axis2 == Direction.Axis.X ? Direction.Axis.Y : Direction.Axis.X;
               };
    }

    public static long offsetPos(long pos, Direction direction, int amount)
    {
        return BlockPos.add(pos,
                            direction.getOffsetX() * amount,
                            direction.getOffsetY() * amount,
                            direction.getOffsetZ() * amount);
    }

    public static long getCompressedPosSide(long pos, Direction side)
    {
        int x = BlockPos.unpackLongX(pos);
        int y = BlockPos.unpackLongY(pos);
        int z = BlockPos.unpackLongZ(pos);
        long val = (1L << side.getId()) << 58;
        val |= (y & 0x3FFFL  ) << 44; // 14 bits for the y
        val |= (z & 0x3FFFFFL) << 22; // 22 bits for the z
        val |= (x & 0x3FFFFFL)      ; // 22 bits for the x

        return val;
    }

    public interface RingPositionTest
    {
        boolean isInsideOrCloserThan(int x, int y, int z, Direction outsideDirection);
    }
}
