package fi.dy.masa.minihud.renderer;

import org.lwjgl.opengl.GL11;
import net.minecraft.client.MinecraftClient;
import net.minecraft.client.render.BufferBuilder;
import net.minecraft.client.render.VertexFormats;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Direction;
import net.minecraft.world.World;
import fi.dy.masa.malilib.util.Color4f;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.util.BlockGridMode;

public class OverlayRendererBlockGrid extends OverlayRendererBase
{
    @Override
    public boolean shouldRender(MinecraftClient mc)
    {
        return RendererToggle.OVERLAY_BLOCK_GRID.getBooleanValue();
    }

    @Override
    public boolean needsUpdate(Entity entity, MinecraftClient mc)
    {
        if (this.lastUpdatePos == null)
        {
            return true;
        }

        return Math.abs(entity.x - this.lastUpdatePos.getX()) > 8 ||
               Math.abs(entity.y - this.lastUpdatePos.getY()) > 8 ||
               Math.abs(entity.z - this.lastUpdatePos.getZ()) > 8;
    }

    @Override
    public void update(Entity entity, MinecraftClient mc)
    {
        Color4f color = Configs.Colors.BLOCK_GRID_OVERLAY_COLOR.getColor();
        int radius = Configs.Generic.BLOCK_GRID_OVERLAY_RADIUS.getIntegerValue();

        RenderObjectBase renderLines = this.renderObjects.get(0);
        BUFFER_1.begin(renderLines.getGlMode(), VertexFormats.POSITION_COLOR);
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

        BUFFER_1.end();
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
                buffer.vertex(x, y, startZ).method_22915(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x, y, endZ).method_22915(color.r, color.g, color.b, color.a).next();
            }
        }

        for (int x = startX; x <= endX; ++x)
        {
            for (int z = startZ; z <= endZ; ++z)
            {
                buffer.vertex(x, startY, z).method_22915(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x, endY, z).method_22915(color.r, color.g, color.b, color.a).next();
            }
        }

        for (int z = startZ; z <= endZ; ++z)
        {
            for (int y = startY; y <= endY; ++y)
            {
                buffer.vertex(startX, y, z).method_22915(color.r, color.g, color.b, color.a).next();
                buffer.vertex(endX, y, z).method_22915(color.r, color.g, color.b, color.a).next();
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
        BlockPos.Mutable posMutable = new BlockPos.Mutable();

        for (int x = startX; x <= endX; ++x)
        {
            for (int z = startZ; z <= endZ; ++z)
            {
                for (int y = startY; y <= endY; ++y)
                {
                    posMutable.set(x, y, z);

                    if (world.isAir(posMutable) == false)
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
        BlockPos.Mutable posMutable = new BlockPos.Mutable();
        BlockPos.Mutable posMutable2 = new BlockPos.Mutable();

        for (int x = startX; x <= endX; ++x)
        {
            for (int z = startZ; z <= endZ; ++z)
            {
                for (int y = startY; y <= endY; ++y)
                {
                    posMutable.set(x, y, z);

                    if (world.isAir(posMutable))
                    {
                        for (Direction side : Direction.values())
                        {
                            posMutable2.set(
                                    posMutable.getX() + side.getOffsetX(),
                                    posMutable.getY() + side.getOffsetY(),
                                    posMutable.getZ() + side.getOffsetZ());

                            if (world.isAir(posMutable2) == false)
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
