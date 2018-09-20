package fi.dy.masa.minihud.renderer;

import org.lwjgl.opengl.GL11;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.RendererToggle;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.vertex.DefaultVertexFormats;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.MathHelper;

public class OverlayRendererRegion extends OverlayRendererBase
{
    protected final BlockPos.MutableBlockPos posMutable = new BlockPos.MutableBlockPos();

    public OverlayRendererRegion()
    {
        this.glLineWidth = 1f;
    }

    @Override
    public boolean shouldRender(Minecraft mc)
    {
        return RendererToggle.OVERLAY_REGION_FILE.getBooleanValue();
    }

    @Override
    public boolean needsUpdate(Entity entity, Minecraft mc)
    {
        int ex = (int) Math.floor(entity.posX);
        int ez = (int) Math.floor(entity.posZ);
        int lx = this.lastUpdatePos.getX();
        int lz = this.lastUpdatePos.getZ();
        return (ex >> 9) != (lx >> 9) || (ez >> 9) != (lz >> 9) || Math.abs(lx - ex) > 16 || Math.abs(lz - ez) > 16;
    }

    @Override
    public void update(Entity entity, Minecraft mc)
    {
        BUFFER_1.begin(GL11.GL_QUADS, DefaultVertexFormats.POSITION_COLOR);
        BUFFER_2.begin(GL11.GL_LINES, DefaultVertexFormats.POSITION_COLOR);

        int rx = MathHelper.floor(entity.posX) & ~0x1FF;
        int rz = MathHelper.floor(entity.posZ) & ~0x1FF;
        BlockPos pos1 = new BlockPos(rx,         0, rz      );
        BlockPos pos2 = new BlockPos(rx + 511, 256, rz + 511);
        int rangeH = (mc.gameSettings.renderDistanceChunks + 1) * 16;
        int color = Configs.Colors.REGION_OVERLAY_COLOR.getIntegerValue();

        RenderUtils.renderVerticalWallsOfLinesWithinRange(BUFFER_1, BUFFER_2, pos1, pos2, rangeH, 256, 16, 16, entity, color);

        BUFFER_1.finishDrawing();
        BUFFER_2.finishDrawing();

        this.renderObjects.get(0).uploadData(BUFFER_1);
        this.renderObjects.get(1).uploadData(BUFFER_2);

        this.lastUpdatePos = new BlockPos(entity);
    }

    @Override
    public void allocateGlResources()
    {
        this.allocateBuffer(GL11.GL_QUADS);
        this.allocateBuffer(GL11.GL_LINES);
    }
}
