package minihud.renderer;

import it.unimi.dsi.fastutil.longs.LongOpenHashSet;

import net.minecraft.entity.Entity;
import net.minecraft.util.math.ChunkPos;
import net.minecraft.util.math.MathHelper;
import net.minecraft.util.math.Vec3d;
import net.minecraft.world.World;

import malilib.render.ShapeRenderUtils;
import malilib.util.data.Color4f;
import malilib.util.game.wrap.EntityWrap;
import malilib.util.game.wrap.GameUtils;
import malilib.util.position.PositionUtils;
import minihud.config.Configs;
import minihud.config.RendererToggle;

public class OverlayRendererSpawnableColumnHeights extends MiniHudOverlayRenderer
{
    private final LongOpenHashSet changedChunks = new LongOpenHashSet();
    private long lastCheckTime = System.nanoTime();

    public void markChunkChanged(int cx, int cz)
    {
        if (RendererToggle.SPAWNABLE_COLUMN_HEIGHTS.isRendererEnabled())
        {
            synchronized (this.changedChunks)
            {
                this.changedChunks.add(ChunkPos.asLong(cx, cz));
            }
        }
    }

    @Override
    public boolean shouldRender()
    {
        return RendererToggle.SPAWNABLE_COLUMN_HEIGHTS.isRendererEnabled();
    }

    @Override
    public boolean needsUpdate(Entity entity)
    {
        int ex = (int) Math.floor(EntityWrap.getX(entity));
        int ez = (int) Math.floor(EntityWrap.getZ(entity));
        int lx = this.lastUpdatePos.getX();
        int lz = this.lastUpdatePos.getZ();

        if (Math.abs(lx - ex) > 8 || Math.abs(lz - ez) > 8)
        {
            return true;
        }

        long currentTime = System.nanoTime();

        if (currentTime - this.lastCheckTime > 1000000000L) // 1 second minimum check interval
        {
            this.lastCheckTime = currentTime;

            int entityCX = ex >> 4;
            int entityCZ = ez >> 4;
            final int chunkRadius = Configs.Generic.SPAWNABLE_COLUMNS_OVERLAY_RADIUS.getIntegerValue() >> 4;

            synchronized (this.changedChunks)
            {
                for (long cpLong : this.changedChunks)
                {
                    int cx = PositionUtils.getChunkPosX(cpLong);
                    int cz = PositionUtils.getChunkPosZ(cpLong);

                    if (Math.abs(entityCX - cx) <= chunkRadius ||
                        Math.abs(entityCZ - cz) <= chunkRadius)
                    {
                        return true;
                    }
                }
            }
        }

        return false;
    }

    @Override
    public void update(Vec3d cameraPos, Entity entity)
    {
        final Color4f color = Configs.Colors.SPAWNABLE_COLUMNS_OVERLAY_COLOR.getColor();
        final int radius = MathHelper.clamp(Configs.Generic.SPAWNABLE_COLUMNS_OVERLAY_RADIUS.getIntegerValue(), 0, 128);

        final int xStart = (int) EntityWrap.getX(entity) - radius;
        final int zStart = (int) EntityWrap.getZ(entity) - radius;
        final int xEnd = xStart + radius * 2;
        final int zEnd = zStart + radius * 2;
        final World world = GameUtils.getClientWorld();

        this.startBuffers();

        for (int x = xStart; x <= xEnd; ++x)
        {
            final double minX = x + 0.25 - cameraPos.x;
            final double maxX = minX + 0.5;

            for (int z = zStart; z <= zEnd; ++z)
            {
                // See WorldEntitySpawner.getRandomChunkPosition()
                int height = MathHelper.roundUp(world.getHeight(x, z) + 1, 16);

                if (height == 0)
                {
                    height = world.getChunk(x << 4, z << 4).getTopFilledSegment() + 15;
                }

                final double minY = height - cameraPos.y;
                final double maxY = minY + 0.09375;
                final double minZ = z + 0.25 - cameraPos.z;
                final double maxZ = minZ + 0.5;

                ShapeRenderUtils.renderBoxHorizontalSideQuads(minX, minY, minZ, maxX, maxY, maxZ, color, this.quadBuilder);
                ShapeRenderUtils.renderBoxTopQuad(minX, minZ, maxX, maxY, maxZ, color, this.quadBuilder);
                ShapeRenderUtils.renderBoxEdgeLines(minX, minY, minZ, maxX, maxY, maxZ, color, this.lineBuilder);
            }
        }

        this.uploadBuffers();

        this.lastCheckTime = System.nanoTime();

        synchronized (this.changedChunks)
        {
            this.changedChunks.clear();
        }
    }
}
