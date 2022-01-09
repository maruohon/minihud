package fi.dy.masa.minihud.renderer.worker;

import org.jetbrains.annotations.NotNull;
import net.minecraft.util.math.ChunkPos;
import net.minecraft.util.math.Vec3i;

public class ChunkTask implements Comparable<ChunkTask>
{
    protected final ChunkPos pos;
    protected final Vec3i referencePosition;
    public final Runnable task;

    public ChunkTask(Runnable task, ChunkPos pos, Vec3i referencePosition)
    {
        this.task = task;
        this.pos = pos;
        this.referencePosition = referencePosition;
    }

    @Override
    public int compareTo(@NotNull ChunkTask other)
    {
        double dist1 = this.distanceSq(this.pos);
        double dist2 = this.distanceSq(other.pos);

        if (dist1 == dist2)
        {
            return 0;
        }

        return dist1 < dist2 ? -1 : 1;
    }

    private double distanceSq(ChunkPos pos)
    {
        double dx = (double) (pos.x << 4) - this.referencePosition.getX();
        double dz = (double) (pos.z << 4) - this.referencePosition.getZ();

        return dx * dx + dz * dz;
    }
}
