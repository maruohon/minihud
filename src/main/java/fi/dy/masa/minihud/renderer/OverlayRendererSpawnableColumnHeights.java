package fi.dy.masa.minihud.renderer;

import java.util.HashSet;
import java.util.Set;
import org.lwjgl.opengl.GL11;
import fi.dy.masa.malilib.util.Color4f;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.RendererToggle;
import net.minecraft.client.Minecraft;
import net.minecraft.client.multiplayer.WorldClient;
import net.minecraft.client.renderer.vertex.DefaultVertexFormats;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.ChunkPos;
import net.minecraft.util.math.MathHelper;
import net.minecraft.world.gen.Heightmap;

public class OverlayRendererSpawnableColumnHeights extends OverlayRendererBase
{
    private static final Set<Long> DIRTY_CHUNKS = new HashSet<>();

    private long lastCheckTime;

    public static void markChunkChanged(int cx, int cz)
    {
        if (RendererToggle.OVERLAY_SPAWNABLE_COLUMN_HEIGHTS.getBooleanValue())
        {
            synchronized (DIRTY_CHUNKS)
            {
                DIRTY_CHUNKS.add(ChunkPos.asLong(cx, cz));
            }
        }
    }

    @Override
    public boolean shouldRender(Minecraft mc)
    {
        return RendererToggle.OVERLAY_SPAWNABLE_COLUMN_HEIGHTS.getBooleanValue();
    }

    @Override
    public boolean needsUpdate(Entity entity, Minecraft mc)
    {
        int ex = (int) Math.floor(entity.posX);
        int ez = (int) Math.floor(entity.posZ);
        int lx = this.lastUpdatePos.getX();
        int lz = this.lastUpdatePos.getZ();

        if (Math.abs(lx - ex) > 8 || Math.abs(lz - ez) > 8)
        {
            return true;
        }

        if (System.currentTimeMillis() - this.lastCheckTime > 1000)
        {
            final int radius = MathHelper.clamp(Configs.Generic.SPAWNABLE_COLUMNS_OVERLAY_RADIUS.getIntegerValue(), 0, 128);
            final int xStart = (((int) entity.posX) >> 4) - radius;
            final int zStart = (((int) entity.posZ) >> 4) - radius;
            final int xEnd = (((int) entity.posX) >> 4) + radius;
            final int zEnd = (((int) entity.posZ) >> 4) + radius;

            synchronized (DIRTY_CHUNKS)
            {
                for (int cx = xStart; cx <= xEnd; ++cx)
                {
                    for (int cz = zStart; cz <= zEnd; ++cz)
                    {
                        if (DIRTY_CHUNKS.contains(ChunkPos.asLong(cx, cz)))
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
    public void update(Entity entity, Minecraft mc)
    {
        final Color4f color = Configs.Colors.SPAWNABLE_COLUMNS_OVERLAY_COLOR.getColor();
        final int radius = MathHelper.clamp(Configs.Generic.SPAWNABLE_COLUMNS_OVERLAY_RADIUS.getIntegerValue(), 0, 128);

        final int xStart = (int) entity.posX - radius;
        final int zStart = (int) entity.posZ - radius;
        final int xEnd = (int) entity.posX + radius;
        final int zEnd = (int) entity.posZ + radius;
        final WorldClient world = mc.world;

        RenderObjectBase renderQuads = this.renderObjects.get(0);
        RenderObjectBase renderLines = this.renderObjects.get(1);
        BUFFER_1.begin(renderQuads.getGlMode(), DefaultVertexFormats.POSITION_COLOR);
        BUFFER_2.begin(renderLines.getGlMode(), DefaultVertexFormats.POSITION_COLOR);

        for (int x = xStart; x <= xEnd; ++x)
        {
            for (int z = zStart; z <= zEnd; ++z)
            {
                // See WorldEntitySpawner.getRandomChunkPosition()
                final int height = world.getHeight(Heightmap.Type.LIGHT_BLOCKING, x, z);
                final double minY = height;
                final double maxY = height + 0.09375;
                final double minX = x + 0.25;
                final double minZ = z + 0.25;
                final double maxX = x + 0.75;
                final double maxZ = z + 0.75;

                fi.dy.masa.malilib.render.RenderUtils.drawBoxHorizontalSidesBatchedQuads(minX, minY, minZ, maxX, maxY, maxZ, color, BUFFER_1);
                fi.dy.masa.malilib.render.RenderUtils.drawBoxTopBatchedQuads(minX, minZ, maxX, maxY, maxZ, color, BUFFER_1);

                fi.dy.masa.malilib.render.RenderUtils.drawBoxAllEdgesBatchedLines(minX, minY, minZ, maxX, maxY, maxZ, color, BUFFER_2);
            }
        }

        BUFFER_1.finishDrawing();
        BUFFER_2.finishDrawing();

        renderQuads.uploadData(BUFFER_1);
        renderLines.uploadData(BUFFER_2);

        this.lastCheckTime = System.currentTimeMillis();

        synchronized (DIRTY_CHUNKS)
        {
            DIRTY_CHUNKS.clear();
        }
    }

    @Override
    public void allocateGlResources()
    {
        this.allocateBuffer(GL11.GL_QUADS);
        this.allocateBuffer(GL11.GL_LINES);
    }
}
