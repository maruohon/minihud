package fi.dy.masa.minihud.renderer;

import org.lwjgl.opengl.GL11;
import fi.dy.masa.malilib.util.Color4f;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.util.BlockGridMode;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.BufferBuilder;
import net.minecraft.client.renderer.vertex.DefaultVertexFormats;
import net.minecraft.entity.Entity;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;

public class OverlayRendererBlockGrid extends OverlayRendererBase
{
    @Override
    public boolean shouldRender(Minecraft mc)
    {
        return RendererToggle.OVERLAY_BLOCK_GRID.getBooleanValue();
    }

    @Override
    public boolean needsUpdate(Entity entity, Minecraft mc)
    {
        if (this.lastUpdatePos == null)
        {
            return true;
        }

        return Math.abs(entity.posX - this.lastUpdatePos.getX()) > 8 ||
               Math.abs(entity.posY - this.lastUpdatePos.getY()) > 8 ||
               Math.abs(entity.posZ - this.lastUpdatePos.getZ()) > 8;
    }

    @Override
    public void update(Entity entity, Minecraft mc)
    {
        Color4f color = Configs.Colors.BLOCK_GRID_OVERLAY_COLOR.getColor();
        int radius = Configs.Generic.BLOCK_GRID_OVERLAY_RADIUS.getIntegerValue();

        RenderObjectBase renderLines = this.renderObjects.get(0);
        BUFFER_1.begin(renderLines.getGlMode(), DefaultVertexFormats.POSITION_COLOR);
        BlockGridMode mode = (BlockGridMode) Configs.Generic.BLOCK_GRID_OVERLAY_MODE.getOptionListValue();

        switch (mode)
        {
            case ALL:
                this.renderLinesAll(this.lastUpdatePos, radius, color, BUFFER_1);
                break;
            case NON_AIR:
                this.renderLinesNonAir(entity.getEntityWorld(), this.lastUpdatePos, radius, color, BUFFER_1);
                break;
            case ADJACENT:
                this.renderLinesAdjacentToNonAir(entity.getEntityWorld(), this.lastUpdatePos, radius, color, BUFFER_1);
                break;
        }

        BUFFER_1.finishDrawing();
        renderLines.uploadData(BUFFER_1);
    }

    @Override
    public void allocateGlResources()
    {
        this.allocateBuffer(GL11.GL_LINES);
    }

    protected void renderLinesAll(BlockPos center, int radius, Color4f color, BufferBuilder buffer)
    {
        int startX = center.getX() - radius;
        int startY = center.getY() - radius;
        int startZ = center.getZ() - radius;
        int endX = center.getX() + radius;
        int endY = center.getY() + radius;
        int endZ = center.getZ() + radius;

        for (int x = startX; x <= endX; ++x)
        {
            for (int y = startY; y <= endY; ++y)
            {
                buffer.pos(x, y, startZ).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x, y, endZ).color(color.r, color.g, color.b, color.a).endVertex();
            }
        }

        for (int x = startX; x <= endX; ++x)
        {
            for (int z = startZ; z <= endZ; ++z)
            {
                buffer.pos(x, startY, z).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x, endY, z).color(color.r, color.g, color.b, color.a).endVertex();
            }
        }

        for (int z = startZ; z <= endZ; ++z)
        {
            for (int y = startY; y <= endY; ++y)
            {
                buffer.pos(startX, y, z).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(endX, y, z).color(color.r, color.g, color.b, color.a).endVertex();
            }
        }
    }

    protected void renderLinesNonAir(World world, BlockPos center, int radius, Color4f color, BufferBuilder buffer)
    {
        int startX = center.getX() - radius;
        int startY = center.getY() - radius;
        int startZ = center.getZ() - radius;
        int endX = center.getX() + radius;
        int endY = center.getY() + radius;
        int endZ = center.getZ() + radius;
        BlockPos.MutableBlockPos posMutable = new BlockPos.MutableBlockPos();

        for (int x = startX; x <= endX; ++x)
        {
            for (int z = startZ; z <= endZ; ++z)
            {
                for (int y = startY; y <= endY; ++y)
                {
                    posMutable.setPos(x, y, z);

                    if (world.isAirBlock(posMutable) == false)
                    {
                        fi.dy.masa.malilib.render.RenderUtils.drawBlockBoundingBoxOutlinesBatchedLines(posMutable, color, 0.001, buffer);
                    }
                }
            }
        }
    }

    protected void renderLinesAdjacentToNonAir(World world, BlockPos center, int radius, Color4f color, BufferBuilder buffer)
    {
        int startX = center.getX() - radius;
        int startY = center.getY() - radius;
        int startZ = center.getZ() - radius;
        int endX = center.getX() + radius;
        int endY = center.getY() + radius;
        int endZ = center.getZ() + radius;
        BlockPos.MutableBlockPos posMutable = new BlockPos.MutableBlockPos();
        BlockPos.MutableBlockPos posMutable2 = new BlockPos.MutableBlockPos();

        for (int x = startX; x <= endX; ++x)
        {
            for (int z = startZ; z <= endZ; ++z)
            {
                for (int y = startY; y <= endY; ++y)
                {
                    posMutable.setPos(x, y, z);

                    if (world.isAirBlock(posMutable))
                    {
                        for (EnumFacing side : EnumFacing.values())
                        {
                            posMutable2.setPos(
                                    posMutable.getX() + side.getXOffset(),
                                    posMutable.getY() + side.getYOffset(),
                                    posMutable.getZ() + side.getZOffset());

                            if (world.isAirBlock(posMutable2) == false)
                            {
                                fi.dy.masa.malilib.render.RenderUtils.drawBlockBoundingBoxOutlinesBatchedLines(posMutable, color, 0.001, buffer);
                                break;
                            }
                        }
                    }
                }
            }
        }
    }
}
