package fi.dy.masa.minihud.renderer;

import org.lwjgl.opengl.GL11;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.RendererToggle;
import net.minecraft.client.MinecraftClient;
import net.minecraft.client.render.VertexFormats;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.MathHelper;

public class OverlayRendererRegion extends OverlayRendererBase
{
    public OverlayRendererRegion()
    {
    }

    @Override
    public boolean shouldRender(MinecraftClient mc)
    {
        return RendererToggle.OVERLAY_REGION_FILE.getBooleanValue();
    }

    @Override
    public boolean needsUpdate(Entity entity, MinecraftClient mc)
    {
        int ex = (int) Math.floor(entity.x);
        int ez = (int) Math.floor(entity.z);
        int lx = this.lastUpdatePos.getX();
        int lz = this.lastUpdatePos.getZ();
        return (ex >> 9) != (lx >> 9) || (ez >> 9) != (lz >> 9) || Math.abs(lx - ex) > 16 || Math.abs(lz - ez) > 16;
    }

    @Override
    public void update(Entity entity, MinecraftClient mc)
    {
        RenderObjectBase renderQuads = this.renderObjects.get(0);
        RenderObjectBase renderLines = this.renderObjects.get(1);
        BUFFER_1.begin(renderQuads.getGlMode(), VertexFormats.POSITION_COLOR);
        BUFFER_2.begin(renderLines.getGlMode(), VertexFormats.POSITION_COLOR);

        int rx = MathHelper.floor(entity.x) & ~0x1FF;
        int rz = MathHelper.floor(entity.z) & ~0x1FF;
        BlockPos pos1 = new BlockPos(rx,         0, rz      );
        BlockPos pos2 = new BlockPos(rx + 511, 256, rz + 511);
        int rangeH = (mc.options.viewDistance + 1) * 16;
        int color = Configs.Colors.REGION_OVERLAY_COLOR.getIntegerValue();

        RenderUtils.renderVerticalWallsOfLinesWithinRange(BUFFER_1, BUFFER_2, pos1, pos2, rangeH, 256, 16, 16, entity, color);

        BUFFER_1.end();
        BUFFER_2.end();

        renderQuads.uploadData(BUFFER_1);
        renderLines.uploadData(BUFFER_2);

        this.lastUpdatePos = new BlockPos(entity);
    }

    @Override
    public void allocateGlResources()
    {
        this.allocateBuffer(GL11.GL_QUADS);
        this.allocateBuffer(GL11.GL_LINES);
    }
}
