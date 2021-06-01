package fi.dy.masa.minihud.renderer;

import net.minecraft.client.MinecraftClient;
import net.minecraft.client.render.BufferBuilder;
import net.minecraft.client.render.VertexFormat;
import net.minecraft.client.render.VertexFormats;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Direction;
import net.minecraft.util.math.Vec3d;
import net.minecraft.world.Heightmap;
import net.minecraft.world.World;
import net.minecraft.world.chunk.WorldChunk;
import fi.dy.masa.malilib.util.Color4f;
import fi.dy.masa.malilib.util.PositionUtils;
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

        return Math.abs(entity.getX() - this.lastUpdatePos.getX()) > 8 ||
               Math.abs(entity.getY() - this.lastUpdatePos.getY()) > 8 ||
               Math.abs(entity.getZ() - this.lastUpdatePos.getZ()) > 8;
    }

    @Override
    public void update(Vec3d cameraPos, Entity entity, MinecraftClient mc)
    {
        Color4f color = Configs.Colors.BLOCK_GRID_OVERLAY_COLOR.getColor();
        int radius = Configs.Generic.BLOCK_GRID_OVERLAY_RADIUS.getIntegerValue();

        RenderObjectBase renderLines = this.renderObjects.get(0);
        BUFFER_1.begin(renderLines.getGlMode(), VertexFormats.POSITION_COLOR);
        BlockGridMode mode = (BlockGridMode) Configs.Generic.BLOCK_GRID_OVERLAY_MODE.getOptionListValue();

        switch (mode)
        {
            case ALL:
                this.renderLinesAll(cameraPos, this.lastUpdatePos, radius, color, BUFFER_1);
                break;
            case NON_AIR:
                this.renderLinesNonAir(cameraPos, entity.getEntityWorld(), this.lastUpdatePos, radius, color, BUFFER_1);
                break;
            case ADJACENT:
                this.renderLinesAdjacentToNonAir(cameraPos, entity.getEntityWorld(), this.lastUpdatePos, radius, color, BUFFER_1);
                break;
        }

        BUFFER_1.end();
        renderLines.uploadData(BUFFER_1);
    }

    @Override
    public void allocateGlResources()
    {
        this.allocateBuffer(VertexFormat.DrawMode.DEBUG_LINES);
    }

    protected void renderLinesAll(Vec3d cameraPos, BlockPos center, int radius, Color4f color, BufferBuilder buffer)
    {
        final double startX = center.getX() - radius - cameraPos.x;
        final double startY = center.getY() - radius - cameraPos.y;
        final double startZ = center.getZ() - radius - cameraPos.z;
        final double endX = center.getX() + radius - cameraPos.x;
        final double endY = center.getY() + radius - cameraPos.y;
        final double endZ = center.getZ() + radius - cameraPos.z;

        for (double x = startX; x <= endX; x += 1.0D)
        {
            for (double y = startY; y <= endY; y += 1.0D)
            {
                buffer.vertex(x, y, startZ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x, y, endZ  ).color(color.r, color.g, color.b, color.a).next();
            }
        }

        for (double x = startX; x <= endX; x += 1.0D)
        {
            for (double z = startZ; z <= endZ; z += 1.0D)
            {
                buffer.vertex(x, startY, z).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x, endY  , z).color(color.r, color.g, color.b, color.a).next();
            }
        }

        for (double z = startZ; z <= endZ; z += 1.0D)
        {
            for (double y = startY; y <= endY; y += 1.0D)
            {
                buffer.vertex(startX, y, z).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(endX  , y, z).color(color.r, color.g, color.b, color.a).next();
            }
        }
    }

    protected void renderLinesNonAir(Vec3d cameraPos, World world, BlockPos center, int radius, Color4f color, BufferBuilder buffer)
    {
        final int startX = center.getX() - radius;
        final int startY = center.getY() - radius;
        final int startZ = center.getZ() - radius;
        final int endX = center.getX() + radius;
        final int endY = center.getY() + radius;
        final int endZ = center.getZ() + radius;
        int lastCX = startX >> 4;
        int lastCZ = startZ >> 4;
        WorldChunk chunk = world.getChunk(lastCX, lastCZ);
        BlockPos.Mutable posMutable = new BlockPos.Mutable();

        for (int x = startX; x <= endX; ++x)
        {
            for (int z = startZ; z <= endZ; ++z)
            {
                int cx = x >> 4;
                int cz = z >> 4;

                if (cx != lastCX || cz != lastCZ)
                {
                    chunk = world.getChunk(cx, cz);
                    lastCX = cx;
                    lastCZ = cz;
                }

                for (int y = startY; y <= endY; ++y)
                {
                    if (y > chunk.sampleHeightmap(Heightmap.Type.WORLD_SURFACE, x, z))
                    {
                        break;
                    }

                    posMutable.set(x, y, z);

                    if (chunk.getBlockState(posMutable).isAir() == false)
                    {
                        fi.dy.masa.malilib.render.RenderUtils.drawBlockBoundingBoxOutlinesBatchedLines(posMutable, cameraPos, color, 0.001, buffer);
                    }
                }
            }
        }
    }

    protected void renderLinesAdjacentToNonAir(Vec3d cameraPos, World world, BlockPos center, int radius, Color4f color, BufferBuilder buffer)
    {
        final int startX = center.getX() - radius;
        final int startY = center.getY() - radius;
        final int startZ = center.getZ() - radius;
        final int endX = center.getX() + radius;
        final int endY = center.getY() + radius;
        final int endZ = center.getZ() + radius;
        int lastCX = startX >> 4;
        int lastCZ = startZ >> 4;
        WorldChunk chunk = world.getChunk(lastCX, lastCZ);
        BlockPos.Mutable posMutable = new BlockPos.Mutable();
        BlockPos.Mutable posMutable2 = new BlockPos.Mutable();

        for (int x = startX; x <= endX; ++x)
        {
            for (int z = startZ; z <= endZ; ++z)
            {
                int cx = x >> 4;
                int cz = z >> 4;

                if (cx != lastCX || cz != lastCZ)
                {
                    chunk = world.getChunk(cx, cz);
                    lastCX = cx;
                    lastCZ = cz;
                }

                for (int y = startY; y <= endY; ++y)
                {
                    posMutable.set(x, y, z);

                    if (chunk.getBlockState(posMutable).isAir())
                    {
                        for (Direction side : PositionUtils.VERTICAL_DIRECTIONS)
                        {
                            posMutable2.set(
                                    posMutable.getX() + side.getOffsetX(),
                                    posMutable.getY() + side.getOffsetY(),
                                    posMutable.getZ() + side.getOffsetZ());

                            if (chunk.getBlockState(posMutable2).isAir() == false)
                            {
                                fi.dy.masa.malilib.render.RenderUtils.drawBlockBoundingBoxOutlinesBatchedLines(posMutable, cameraPos, color, 0.001, buffer);
                                break;
                            }
                        }

                        for (Direction side : PositionUtils.HORIZONTAL_DIRECTIONS)
                        {
                            posMutable2.set(
                                    posMutable.getX() + side.getOffsetX(),
                                    posMutable.getY() + side.getOffsetY(),
                                    posMutable.getZ() + side.getOffsetZ());

                            if (world.isAir(posMutable2) == false)
                            {
                                fi.dy.masa.malilib.render.RenderUtils.drawBlockBoundingBoxOutlinesBatchedLines(posMutable, cameraPos, color, 0.001, buffer);
                                break;
                            }
                        }
                    }
                }
            }
        }
    }
}
