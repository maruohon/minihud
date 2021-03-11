package fi.dy.masa.minihud.renderer;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import org.lwjgl.opengl.GL11;
import com.google.common.collect.ArrayListMultimap;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.vertex.DefaultVertexFormats;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Vec3d;
import fi.dy.masa.malilib.render.ShapeRenderUtils;
import fi.dy.masa.malilib.render.TextRenderUtils;
import fi.dy.masa.malilib.render.overlay.BaseRenderObject;
import fi.dy.masa.malilib.util.data.Color4f;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.data.DataStorage;

public class OverlayRendererSpawnerPositions extends OverlayRendererBase
{
    private static OverlayRendererSpawnerPositions instance;

    private boolean wasDisabled = true;
    private final List<BlockPos> textPositions = new ArrayList<>();

    public OverlayRendererSpawnerPositions()
    {
        instance = this;
    }

    @Override
    public boolean shouldRender(Minecraft mc)
    {
        boolean render = RendererToggle.OVERLAY_SPAWNER_POSITIONS.isRendererEnabled() &&
                         mc.world.provider.isSurfaceWorld();
        this.wasDisabled |= ! render;
        return render;
    }

    @Override
    public boolean needsUpdate(Entity entity, Minecraft mc)
    {
        int hysteresis = 16;

        return this.wasDisabled || DataStorage.getInstance().areSpawnerPositionsDirty() ||
               Math.abs(entity.posX - this.lastUpdatePos.getX()) > hysteresis ||
               Math.abs(entity.posZ - this.lastUpdatePos.getZ()) > hysteresis;
    }

    @Override
    public void update(Vec3d cameraPos, Entity entity, Minecraft mc)
    {
        BaseRenderObject renderQuads = this.renderObjects.get(0);
        BaseRenderObject renderLines = this.renderObjects.get(1);
        BUFFER_1.begin(renderQuads.getGlMode(), DefaultVertexFormats.POSITION_COLOR);
        BUFFER_2.begin(renderLines.getGlMode(), DefaultVertexFormats.POSITION_COLOR);

        this.renderPositionsWithinChunkRange(this.lastUpdatePos, 8, 2, cameraPos);

        BUFFER_1.finishDrawing();
        BUFFER_2.finishDrawing();

        renderQuads.uploadData(BUFFER_1);
        renderLines.uploadData(BUFFER_2);

        this.wasDisabled = false;
    }

    @Override
    public void allocateGlResources()
    {
        this.allocateBuffer(GL11.GL_QUADS);
        this.allocateBuffer(GL11.GL_LINES);
    }

    protected void renderPositionsWithinChunkRange(BlockPos center, int chunkRange, int textRenderChunkRange, Vec3d cameraPos)
    {
        Color4f colorQuads = Configs.Colors.REGION_OVERLAY_COLOR.getColor();
        Color4f colorLines = colorQuads.withAlpha(1.0f);
        ArrayListMultimap<Long, BlockPos> map = DataStorage.getInstance().getSpawnerPositions();
        int centerChunkX = center.getX() >> 4;
        int centerChunkZ = center.getZ() >> 4;

        this.textPositions.clear();

        for (int cx = centerChunkX - chunkRange; cx <= centerChunkX + chunkRange; ++cx)
        {
            for (int cz = centerChunkZ - chunkRange; cz <= centerChunkZ + chunkRange; ++cz)
            {
                long cp = (long) cz << 32 | (((long) cx) & 0xFFFFFFFFL);
                Collection<BlockPos> positions = map.get(cp);
                this.renderPositions(positions, colorQuads, colorLines, cameraPos);

                if (Math.abs(cx - centerChunkX) <= textRenderChunkRange &&
                    Math.abs(cz - centerChunkZ) <= textRenderChunkRange)
                {
                    this.textPositions.addAll(positions);
                }
            }
        }
    }

    protected void renderPositions(Collection<BlockPos> positions, Color4f colorQuads, Color4f colorLines, Vec3d cameraPos)
    {
        for (BlockPos pos : positions)
        {
            ShapeRenderUtils.renderBlockPosSideQuads(pos, 0.001, colorQuads, BUFFER_1, cameraPos);
            ShapeRenderUtils.renderBlockPosEdgeLines(pos, 0.001, colorLines, BUFFER_2, cameraPos);
        }
    }

    public static void renderPositionText(double dx, double dy, double dz)
    {
        final float scale = 0.025f;

        for (BlockPos pos : instance.textPositions)
        {
            String str = String.format("%d, %d, %d", pos.getX(), pos.getY(), pos.getZ());
            double x = pos.getX() + 0.5;
            double y = pos.getY() + 1.5;
            double z = pos.getZ() + 0.5;
            TextRenderUtils.renderTextPlate(Arrays.asList(str), x - dx, y - dy, z - dz, scale);
        }
    }
}
