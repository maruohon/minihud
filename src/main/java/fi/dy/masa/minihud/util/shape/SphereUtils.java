package fi.dy.masa.minihud.util.shape;

import java.util.function.Function;
import javax.annotation.Nullable;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Direction;
import net.minecraft.util.math.Vec3d;
import it.unimi.dsi.fastutil.longs.LongOpenHashSet;

public class SphereUtils
{
    public static boolean movePositionToRing(BlockPos.Mutable posMutable,
                                             Direction moveDirection,
                                             double radius,
                                             RingPositionTest test)
    {
        final int failsafeMax = (int) radius + 2;
        final int incX = moveDirection.getOffsetX();
        final int incY = moveDirection.getOffsetY();
        final int incZ = moveDirection.getOffsetZ();
        int x = posMutable.getX();
        int y = posMutable.getY();
        int z = posMutable.getZ();
        int nextX = x;
        int nextY = y;
        int nextZ = z;
        int failsafe = 0;

        while (test.isInsideOrCloserThan(nextX, nextY, nextZ, moveDirection) && ++failsafe < failsafeMax)
        {
            x = nextX;
            y = nextY;
            z = nextZ;
            nextX += incX;
            nextY += incY;
            nextZ += incZ;
        }

        // Successfully entered the loop at least once
        if (failsafe > 0)
        {
            posMutable.set(x, y, z);
            return true;
        }

        return false;
    }

    public static void addPositionsOnHorizontalBlockRing(LongOpenHashSet positions,
                                                         BlockPos.Mutable mutablePos,
                                                         RingPositionTest test,
                                                         double radius)
    {
        Function<Direction, Direction> nextDirectionFunction = SphereUtils::getNextHorizontalDirection;
        Direction startDirection = Direction.EAST;
        addPositionsOnBlockRing(positions, mutablePos, startDirection, test, nextDirectionFunction, radius);
    }

    public static void addPositionsOnVerticalBlockRing(LongOpenHashSet positions,
                                                       BlockPos.Mutable mutablePos,
                                                       Direction mainAxis,
                                                       RingPositionTest test,
                                                       double radius)
    {
        Function<Direction, Direction> nextDirectionFunction = (dir) -> SphereUtils.getNextVerticalRingDirection(dir, mainAxis);
        Direction startDirection = Direction.UP;
        addPositionsOnBlockRing(positions, mutablePos, startDirection, test, nextDirectionFunction, radius);
    }

    public static void addPositionsOnBlockRing(LongOpenHashSet positions,
                                               BlockPos.Mutable mutablePos,
                                               Direction startDirection,
                                               RingPositionTest test,
                                               Function<Direction, Direction> nextDirectionFunction,
                                               double radius)
    {
        if (movePositionToRing(mutablePos, startDirection, radius, test))
        {
            final BlockPos firstPos = mutablePos.toImmutable();
            Direction direction = startDirection;
            int failsafe = (int) (2.5 * Math.PI * radius); // a bit over the circumference

            positions.add(firstPos.asLong());

            while (--failsafe > 0)
            {
                direction = getNextPositionOnBlockRing(mutablePos, direction, test, nextDirectionFunction);

                if (direction == null || mutablePos.equals(firstPos))
                {
                    break;
                }

                positions.add(mutablePos.asLong());
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
    protected static Direction getNextHorizontalDirection(Direction dirIn)
    {
        return dirIn.rotateYCounterclockwise();
    }

    /**
     * Returns the next direction in sequence, rotating up to north
     */
    protected static Direction getNextVerticalRingDirection(Direction currentDirection, Direction mainAxis)
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

    public interface RingPositionTest
    {
        boolean isInsideOrCloserThan(int x, int y, int z, Direction outsideDirection);
    }
}
