package fi.dy.masa.minihud.util;

import java.util.function.LongConsumer;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Direction;
import net.minecraft.util.math.MathHelper;
import net.minecraft.util.math.Vec3d;

public class RayTracer
{
    protected final Vec3d start;
    protected final Vec3d end;
    protected final int endBlockX;
    protected final int endBlockY;
    protected final int endBlockZ;
    protected double x;
    protected double y;
    protected double z;
    protected int blockX;
    protected int blockY;
    protected int blockZ;
    protected Direction side = Direction.UP;

    public RayTracer(BlockPos start, BlockPos end)
    {
        this(new Vec3d(start.getX() + 0.5, start.getY() + 0.5, start.getZ() + 0.5),
             new Vec3d(end.getX() + 0.5, end.getY() + 0.5, end.getZ() + 0.5));
    }

    public RayTracer(Vec3d start, Vec3d end)
    {
        this.start = start;
        this.end = end;
        this.x = start.x;
        this.y = start.y;
        this.z = start.z;
        this.endBlockX = MathHelper.floor(end.x);
        this.endBlockY = MathHelper.floor(end.y);
        this.endBlockZ = MathHelper.floor(end.z);
        this.blockX = MathHelper.floor(start.x);
        this.blockY = MathHelper.floor(start.y);
        this.blockZ = MathHelper.floor(start.z);
    }

    public boolean advance()
    {
        if (Double.isNaN(this.x) || Double.isNaN(this.y) || Double.isNaN(this.z))
        {
            return true;
        }

        if (this.blockX == this.endBlockX && this.blockY == this.endBlockY && this.blockZ == this.endBlockZ)
        {
            return true;
        }

        boolean positiveX = this.endBlockX > this.blockX;
        boolean positiveY = this.endBlockY > this.blockY;
        boolean positiveZ = this.endBlockZ > this.blockZ;
        double distToEndX = this.end.x - this.x;
        double distToEndY = this.end.y - this.y;
        double distToEndZ = this.end.z - this.z;
        double nextX = 999.0D;
        double nextY = 999.0D;
        double nextZ = 999.0D;
        double relStepX = 999.0;
        double relStepY = 999.0;
        double relStepZ = 999.0;

        if (positiveX)
        {
            nextX = (double) this.blockX + 1.0D;
            relStepX = (nextX - this.x) / distToEndX;
        }
        else if (this.endBlockX < this.blockX)
        {
            nextX = this.blockX;
            relStepX = (nextX - this.x) / distToEndX;
        }

        if (positiveY)
        {
            nextY = (double) this.blockY + 1.0D;
            relStepY = (nextY - this.y) / distToEndY;
        }
        else if (this.endBlockY < this.blockY)
        {
            nextY = this.blockY;
            relStepY = (nextY - this.y) / distToEndY;
        }

        if (positiveZ)
        {
            nextZ = (double) this.blockZ + 1.0D;
            relStepZ = (nextZ - this.z) / distToEndZ;
        }
        else if (this.endBlockZ < this.blockZ)
        {
            nextZ = this.blockZ;
            relStepZ = (nextZ - this.z) / distToEndZ;
        }

        if (relStepX == -0.0D) { relStepX = -1.0E-4D; }
        if (relStepY == -0.0D) { relStepY = -1.0E-4D; }
        if (relStepZ == -0.0D) { relStepZ = -1.0E-4D; }

        if (relStepX <= relStepY && relStepX <= relStepZ)
        {
            this.side = positiveX ? Direction.WEST : Direction.EAST;
            this.x = nextX;
            this.y += distToEndY * relStepX;
            this.z += distToEndZ * relStepX;
        }
        else if (relStepY <= relStepZ && relStepY <= relStepX)
        {
            this.side = positiveY ? Direction.DOWN : Direction.UP;
            this.x += distToEndX * relStepY;
            this.y = nextY;
            this.z += distToEndZ * relStepY;
        }
        else //if (relStepZ <= relStepX && relStepZ <= relStepY)
        {
            this.side = positiveZ ? Direction.NORTH : Direction.SOUTH;
            this.x += distToEndX * relStepZ;
            this.y += distToEndY * relStepZ;
            this.z = nextZ;
        }

        this.blockX = MathHelper.floor(this.x) - (this.side == Direction.EAST  ? 1 : 0);
        this.blockY = MathHelper.floor(this.y) - (this.side == Direction.UP    ? 1 : 0);
        this.blockZ = MathHelper.floor(this.z) - (this.side == Direction.SOUTH ? 1 : 0);

        return false;
    }

    public Vec3d getPosition()
    {
        return new Vec3d(this.x, this.y, this.z);
    }

    public Direction getSide()
    {
        return this.side;
    }

    public void getBlockPosition(LongConsumer consumer)
    {
        long pos = BlockPos.asLong(this.blockX, this.blockY, this.blockZ);
        consumer.accept(pos);
    }

    public void iterateAllPositions(LongConsumer consumer)
    {
        this.getBlockPosition(consumer);

        while (this.advance() == false)
        {
            this.getBlockPosition(consumer);
        }
    }
}
