package fi.dy.masa.minihud.renderer;

import java.util.HashSet;
import java.util.Set;
import org.lwjgl.opengl.GL11;
import net.minecraft.client.Minecraft;
import net.minecraft.client.multiplayer.WorldClient;
import net.minecraft.client.renderer.vertex.DefaultVertexFormats;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.ChunkPos;
import net.minecraft.util.math.MathHelper;
import net.minecraft.util.math.Vec3d;
import fi.dy.masa.malilib.render.overlay.RenderObjectBase;
import fi.dy.masa.malilib.util.data.Color4f;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.RendererToggle;

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
    public void update(Vec3d cameraPos, Entity entity, Minecraft mc)
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
