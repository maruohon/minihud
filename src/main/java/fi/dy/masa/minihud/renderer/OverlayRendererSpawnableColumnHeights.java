package fi.dy.masa.minihud.renderer;

import java.util.HashSet;
import java.util.Set;
import net.minecraft.client.MinecraftClient;
import net.minecraft.client.render.VertexFormats;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.ChunkPos;
import net.minecraft.util.math.MathHelper;
import net.minecraft.util.math.Vec3d;
import net.minecraft.world.Heightmap;
import net.minecraft.world.World;
import fi.dy.masa.malilib.util.Color4f;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.RendererToggle;

public class OverlayRendererSpawnableColumnHeights extends OverlayRendererBase
{
    private static final Set<Long> DIRTY_CHUNKS = new HashSet<>();

    private final BlockPos.Mutable posMutable = new BlockPos.Mutable();
    private long lastCheckTime;

    public static void markChunkChanged(int cx, int cz)
    {
        if (RendererToggle.OVERLAY_SPAWNABLE_COLUMN_HEIGHTS.getBooleanValue())
        {
            synchronized (DIRTY_CHUNKS)
            {
                DIRTY_CHUNKS.add(ChunkPos.toLong(cx, cz));
            }
        }
    }

    @Override
    public boolean shouldRender(MinecraftClient mc)
    {
        return RendererToggle.OVERLAY_SPAWNABLE_COLUMN_HEIGHTS.getBooleanValue();
    }

    @Override
    public boolean needsUpdate(Entity entity, MinecraftClient mc)
    {
        int ex = (int) Math.floor(entity.getX());
        int ez = (int) Math.floor(entity.getZ());
        int lx = this.lastUpdatePos.getX();
        int lz = this.lastUpdatePos.getZ();

        if (Math.abs(lx - ex) > 8 || Math.abs(lz - ez) > 8)
        {
            return true;
        }

        if (System.currentTimeMillis() - this.lastCheckTime > 1000)
        {
            final int radius = MathHelper.clamp(Configs.Generic.SPAWNABLE_COLUMNS_OVERLAY_RADIUS.getIntegerValue(), 0, 128);
            final int xStart = (((int) entity.getX() - radius) >> 4);
            final int zStart = (((int) entity.getZ() - radius) >> 4);
            final int xEnd = (((int) entity.getX() + radius) >> 4);
            final int zEnd = (((int) entity.getZ() + radius) >> 4);

            synchronized (DIRTY_CHUNKS)
            {
                for (int cx = xStart; cx <= xEnd; ++cx)
                {
                    for (int cz = zStart; cz <= zEnd; ++cz)
                    {
                        if (DIRTY_CHUNKS.contains(ChunkPos.toLong(cx, cz)))
                        {
                            return true;
                        }
                    }
                }
            }

            this.lastCheckTime = System.currentTimeMillis();
        }

        return false;
    }

    @Override
    public void update(Vec3d cameraPos, Entity entity, MinecraftClient mc)
    {
        final Color4f color = Configs.Colors.SPAWNABLE_COLUMNS_OVERLAY_COLOR.getColor();
        final int radius = MathHelper.clamp(Configs.Generic.SPAWNABLE_COLUMNS_OVERLAY_RADIUS.getIntegerValue(), 0, 128);

        final int xStart = (int) entity.getX() - radius;
        final int zStart = (int) entity.getZ() - radius;
        final int xEnd = (int) entity.getX() + radius;
        final int zEnd = (int) entity.getZ() + radius;
        final World world = mc.world;

        RenderObjectBase renderQuads = this.renderObjects.get(0);
        RenderObjectBase renderLines = this.renderObjects.get(1);
        BUFFER_1.begin(renderQuads.getGlMode(), VertexFormats.POSITION_COLOR);
        BUFFER_2.begin(renderLines.getGlMode(), VertexFormats.POSITION_COLOR);

        for (int x = xStart; x <= xEnd; ++x)
        {
            final double minX = x + 0.25 - cameraPos.x;
            final double maxX = minX + 0.5;

            for (int z = zStart; z <= zEnd; ++z)
            {
                // See WorldEntitySpawner.getRandomChunkPosition()
                final int height = world.getWorldChunk(this.posMutable.set(x, 0, z)).sampleHeightmap(Heightmap.Type.WORLD_SURFACE, x, z) + 1;
                final double minY = height - cameraPos.y;
                final double maxY = minY + 0.09375;
                final double minZ = z + 0.25 - cameraPos.z;
                final double maxZ = minZ + 0.5;

                fi.dy.masa.malilib.render.RenderUtils.drawBoxHorizontalSidesBatchedQuads(minX, minY, minZ, maxX, maxY, maxZ, color, BUFFER_1);
                fi.dy.masa.malilib.render.RenderUtils.drawBoxTopBatchedQuads(minX, minZ, maxX, maxY, maxZ, color, BUFFER_1);

                fi.dy.masa.malilib.render.RenderUtils.drawBoxAllEdgesBatchedLines(minX, minY, minZ, maxX, maxY, maxZ, color, BUFFER_2);
            }
        }

        BUFFER_1.end();
        BUFFER_2.end();

        renderQuads.uploadData(BUFFER_1);
        renderLines.uploadData(BUFFER_2);

        this.lastCheckTime = System.currentTimeMillis();

        synchronized (DIRTY_CHUNKS)
        {
            DIRTY_CHUNKS.clear();
        }
    }
}
