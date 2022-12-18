package minihud.renderer;

import net.minecraft.client.renderer.vertex.DefaultVertexFormats;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.MathHelper;
import net.minecraft.util.math.Vec3d;

import malilib.render.overlay.BaseRenderObject;
import malilib.util.data.Color4f;
import malilib.util.game.wrap.EntityWrap;
import minihud.config.Configs;
import minihud.config.RendererToggle;

public class OverlayRendererRegion extends MiniHudOverlayRenderer
{
    @Override
    public boolean shouldRender()
    {
        return RendererToggle.REGION_FILE.isRendererEnabled();
    }

    @Override
    public boolean needsUpdate(Entity entity)
    {
        int ex = (int) Math.floor(EntityWrap.getX(entity));
        int ez = (int) Math.floor(EntityWrap.getZ(entity));
        int lx = this.lastUpdatePos.getX();
        int lz = this.lastUpdatePos.getZ();

        return (ex >> 9) != (lx >> 9) || (ez >> 9) != (lz >> 9) || Math.abs(lx - ex) > 16 || Math.abs(lz - ez) > 16;
    }

    @Override
    public void update(Vec3d cameraPos, Entity entity)
    {
        BaseRenderObject renderQuads = this.renderObjects.get(0);
        BaseRenderObject renderLines = this.renderObjects.get(1);
        BUFFER_1.begin(renderQuads.getGlMode(), DefaultVertexFormats.POSITION_COLOR);
        BUFFER_2.begin(renderLines.getGlMode(), DefaultVertexFormats.POSITION_COLOR);

        int rx = MathHelper.floor(EntityWrap.getX(entity)) & ~0x1FF;
        int rz = MathHelper.floor(EntityWrap.getZ(entity)) & ~0x1FF;
        BlockPos pos1 = new BlockPos(rx,         0, rz      );
        BlockPos pos2 = new BlockPos(rx + 511, 256, rz + 511);
        Color4f color = Configs.Colors.REGION_OVERLAY_COLOR.getColor();

        RenderUtils.renderWallsWithLines(pos1, pos2, cameraPos, 16, 16, true, color, BUFFER_1, BUFFER_2);

        BUFFER_1.finishDrawing();
        BUFFER_2.finishDrawing();

        renderQuads.uploadData(BUFFER_1);
        renderLines.uploadData(BUFFER_2);
    }
}
