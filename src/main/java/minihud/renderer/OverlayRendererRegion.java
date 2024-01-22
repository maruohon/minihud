package minihud.renderer;

import net.minecraft.entity.Entity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Vec3d;

import malilib.util.MathUtils;
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
        int ex = MathUtils.floor(EntityWrap.getX(entity));
        int ez = MathUtils.floor(EntityWrap.getZ(entity));
        int lx = this.lastUpdatePos.getX();
        int lz = this.lastUpdatePos.getZ();

        return (ex >> 9) != (lx >> 9) || (ez >> 9) != (lz >> 9) || Math.abs(lx - ex) > 16 || Math.abs(lz - ez) > 16;
    }

    @Override
    public void update(Vec3d cameraPos, Entity entity)
    {
        Color4f color = Configs.Colors.REGION_OVERLAY_COLOR.getColor();
        int rx = MathUtils.floor(EntityWrap.getX(entity)) & ~0x1FF;
        int rz = MathUtils.floor(EntityWrap.getZ(entity)) & ~0x1FF;
        BlockPos pos1 = new BlockPos(rx,         0, rz      );
        BlockPos pos2 = new BlockPos(rx + 511, 256, rz + 511);

        this.startBuffers();

        RenderUtils.renderWallsWithLines(pos1, pos2, cameraPos, 16, 16, true, color, this.quadBuilder, this.lineBuilder);

        this.uploadBuffers();
    }
}
