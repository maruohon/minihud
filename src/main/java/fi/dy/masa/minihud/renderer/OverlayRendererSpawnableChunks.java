package fi.dy.masa.minihud.renderer;

import javax.annotation.Nullable;
import org.lwjgl.opengl.GL11;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.RendererToggle;
import net.minecraft.client.MinecraftClient;
import net.minecraft.client.render.VertexFormats;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.BlockPos;

public class OverlayRendererSpawnableChunks extends OverlayRendererBase
{
    @Nullable public static BlockPos newPos;
    public static double overlayTopY;

    protected final RendererToggle toggle;
    protected double topY;

    public OverlayRendererSpawnableChunks(RendererToggle toggle)
    {
        this.toggle = toggle;
    }

    @Override
    public boolean shouldRender(MinecraftClient mc)
    {
        return this.toggle.getBooleanValue();
    }

    @Override
    public boolean needsUpdate(Entity entity, MinecraftClient mc)
    {
        /*
        if (this.topY != overlayTopY)
        {
            return true;
        }
        */

        if (this.toggle == RendererToggle.OVERLAY_SPAWNABLE_CHUNKS_FIXED)
        {
            return newPos != null;
        }
        else
        {
            int ex = ((int) Math.floor(entity.x)) >> 4;
            int ez = ((int) Math.floor(entity.z)) >> 4;
            int lx = this.lastUpdatePos.getX();
            int lz = this.lastUpdatePos.getZ();

            return ex != lx || ez != lz;
        }
    }

    @Override
    public void update(Entity entity, MinecraftClient mc)
    {
        BlockPos posCenter;

        if (this.toggle == RendererToggle.OVERLAY_SPAWNABLE_CHUNKS_FIXED)
        {
            if (newPos != null)
            {
                posCenter = newPos;
                newPos = null;
            }
            else
            {
                posCenter = new BlockPos(this.lastUpdatePos.getX() << 4, 0, this.lastUpdatePos.getZ() << 4);
            }
        }
        else
        {
            posCenter = new BlockPos(entity);
        }

        RenderObjectBase renderQuads = this.renderObjects.get(0);
        RenderObjectBase renderLines = this.renderObjects.get(1);
        BUFFER_1.begin(renderQuads.getGlMode(), VertexFormats.POSITION_COLOR);
        BUFFER_2.begin(renderLines.getGlMode(), VertexFormats.POSITION_COLOR);

        int centerX = posCenter.getX() >> 4;
        int centerZ = posCenter.getZ() >> 4;
        int r = 7;
        final int color = this.toggle == RendererToggle.OVERLAY_SPAWNABLE_CHUNKS_FIXED ?
                Configs.Colors.SPAWNABLE_CHUNKS_FIXED_OVERLAY_COLOR.getIntegerValue() :
                Configs.Colors.SPAWNABLE_CHUNKS_PLAYER_OVERLAY_COLOR.getIntegerValue();

        //this.topY = overlayTopY;
        this.topY = 256;

        BlockPos pos1 = new BlockPos( (centerX - r    ) << 4,              0,  (centerZ - r    ) << 4     );
        BlockPos pos2 = new BlockPos(((centerX + r + 1) << 4) - 1, this.topY, ((centerZ + r + 1) << 4) - 1);

        RenderUtils.renderVerticalWallsOfLinesWithinRange(BUFFER_1, BUFFER_2, pos1, pos2, 256, 256, 16, 16, entity, color);

        BUFFER_1.end();
        BUFFER_2.end();

        renderQuads.uploadData(BUFFER_1);
        renderLines.uploadData(BUFFER_2);

        this.lastUpdatePos = new BlockPos(centerX, 0, centerZ);
    }

    @Override
    public void allocateGlResources()
    {
        this.allocateBuffer(GL11.GL_QUADS);
        this.allocateBuffer(GL11.GL_LINES);
    }
}
