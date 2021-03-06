package fi.dy.masa.minihud.renderer;

import net.minecraft.client.MinecraftClient;
import net.minecraft.client.render.VertexFormats;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.MathHelper;
import net.minecraft.util.math.Vec3d;
import net.minecraft.world.World;
import fi.dy.masa.malilib.util.Color4f;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.RendererToggle;

public class OverlayRendererRegion extends OverlayRendererBase
{
    protected static boolean needsUpdate = true;

    public static void setNeedsUpdate()
    {
        needsUpdate = true;
    }

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
        if (needsUpdate)
        {
            return true;
        }

        int ex = (int) Math.floor(entity.getX());
        int ez = (int) Math.floor(entity.getZ());
        int lx = this.lastUpdatePos.getX();
        int lz = this.lastUpdatePos.getZ();

        return (ex >> 9) != (lx >> 9) || (ez >> 9) != (lz >> 9) || Math.abs(lx - ex) > 16 || Math.abs(lz - ez) > 16;
    }

    @Override
    public void update(Vec3d cameraPos, Entity entity, MinecraftClient mc)
    {
        RenderObjectBase renderQuads = this.renderObjects.get(0);
        RenderObjectBase renderLines = this.renderObjects.get(1);
        BUFFER_1.begin(renderQuads.getGlMode(), VertexFormats.POSITION_COLOR);
        BUFFER_2.begin(renderLines.getGlMode(), VertexFormats.POSITION_COLOR);

        World world = entity.getEntityWorld();
        int minY = world != null ? world.getBottomY() : -64;
        int maxY = world != null ? world.getTopY() : 320;
        int rx = MathHelper.floor(entity.getX()) & ~0x1FF;
        int rz = MathHelper.floor(entity.getZ()) & ~0x1FF;
        BlockPos pos1 = new BlockPos(rx,       minY, rz      );
        BlockPos pos2 = new BlockPos(rx + 511, maxY, rz + 511);
        Color4f color = Configs.Colors.REGION_OVERLAY_COLOR.getColor();

        RenderUtils.renderWallsWithLines(pos1, pos2, cameraPos, 16, 16, true, color, BUFFER_1, BUFFER_2);

        BUFFER_1.end();
        BUFFER_2.end();

        renderQuads.uploadData(BUFFER_1);
        renderLines.uploadData(BUFFER_2);

        needsUpdate = false;
    }
}
