package minihud.renderer;

import org.lwjgl.opengl.GL11;

import net.minecraft.block.material.Material;
import net.minecraft.entity.Entity;
import net.minecraft.world.World;
import net.minecraft.world.chunk.Chunk;

import malilib.render.ShapeRenderUtils;
import malilib.render.buffer.VertexBuilder;
import malilib.util.data.Color4f;
import malilib.util.game.wrap.EntityWrap;
import malilib.util.game.wrap.GameUtils;
import malilib.util.position.BlockPos;
import malilib.util.position.Direction;
import malilib.util.position.Vec3d;
import minihud.config.Configs;
import minihud.config.RendererToggle;
import minihud.util.value.BlockGridMode;

public class OverlayRendererBlockGrid extends MiniHudOverlayRenderer
{
    @Override
    public boolean shouldRender()
    {
        return RendererToggle.BLOCK_GRID.isRendererEnabled();
    }

    @Override
    public boolean needsUpdate(Entity entity)
    {
        if (this.lastUpdatePos == null)
        {
            return true;
        }

        return Math.abs(EntityWrap.getX(entity) - this.lastUpdatePos.getX()) > 8 ||
               Math.abs(EntityWrap.getY(entity) - this.lastUpdatePos.getY()) > 8 ||
               Math.abs(EntityWrap.getZ(entity) - this.lastUpdatePos.getZ()) > 8;
    }

    @Override
    public void allocateGlResources()
    {
        this.outlineRenderer = this.allocateBuffer(GL11.GL_LINES);
    }

    @Override
    protected void uploadBuffers()
    {
        this.outlineRenderer.uploadData(this.lineBuilder);
        this.needsUpdate = false;
    }

    @Override
    protected void startBuffers()
    {
        this.lineBuilder.start();
    }

    @Override
    public void draw()
    {
        this.preRender();
        this.outlineRenderer.draw();
        this.postRender();
    }

    @Override
    public void update(Vec3d cameraPos, Entity entity)
    {
        World world = GameUtils.getClientWorld();
        Color4f color = Configs.Colors.BLOCK_GRID_OVERLAY_COLOR.getColor();
        BlockGridMode mode = Configs.Generic.BLOCK_GRID_OVERLAY_MODE.getValue();
        int radius = Configs.Generic.BLOCK_GRID_OVERLAY_RADIUS.getIntegerValue();

        this.startBuffers();

        if (mode == BlockGridMode.ALL)
        {
            this.renderLinesAll(cameraPos, this.lastUpdatePos, radius, color, this.lineBuilder);
        }
        else if (mode == BlockGridMode.NON_AIR)
        {
            this.renderLinesNonAir(cameraPos, world, this.lastUpdatePos, radius, color, this.lineBuilder);
        }
        else if (mode == BlockGridMode.ADJACENT)
        {
            this.renderLinesAdjacentToNonAir(cameraPos, world, this.lastUpdatePos, radius, color, this.lineBuilder);
        }

        this.uploadBuffers();
    }

    protected void renderLinesAll(Vec3d cameraPos,
                                  BlockPos center,
                                  int radius,
                                  Color4f color,
                                  VertexBuilder builder)
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
                builder.posColor(x, y, startZ, color);
                builder.posColor(x, y, endZ  , color);
            }
        }

        for (double x = startX; x <= endX; x += 1.0D)
        {
            for (double z = startZ; z <= endZ; z += 1.0D)
            {
                builder.posColor(x, startY, z, color);
                builder.posColor(x, endY  , z, color);
            }
        }

        for (double z = startZ; z <= endZ; z += 1.0D)
        {
            for (double y = startY; y <= endY; y += 1.0D)
            {
                builder.posColor(startX, y, z, color);
                builder.posColor(endX  , y, z, color);
            }
        }
    }

    protected void renderLinesNonAir(Vec3d cameraPos,
                                     World world,
                                     BlockPos center,
                                     int radius,
                                     Color4f color,
                                     VertexBuilder builder)
    {
        final int startX = center.getX() - radius;
        final int startY = center.getY() - radius;
        final int startZ = center.getZ() - radius;
        final int endX = center.getX() + radius;
        final int endY = center.getY() + radius;
        final int endZ = center.getZ() + radius;
        int lastCX = startX >> 4;
        int lastCZ = startZ >> 4;
        Chunk chunk = world.getChunk(lastCX, lastCZ);
        BlockPos.MutBlockPos posMutable = new BlockPos.MutBlockPos();

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

                int height = chunk.getHeightValue(x & 0xF, z & 0xF);

                for (int y = startY; y <= endY; ++y)
                {
                    if (y > height)
                    {
                        break;
                    }

                    posMutable.set(x, y, z);

                    if (chunk.getBlockState(x, y, z).getMaterial() != Material.AIR)
                    {
                        ShapeRenderUtils.renderBlockPosEdgeLines(posMutable, 0.001, color, cameraPos, builder);
                    }
                }
            }
        }
    }

    protected void renderLinesAdjacentToNonAir(Vec3d cameraPos,
                                               World world,
                                               BlockPos center,
                                               int radius,
                                               Color4f color,
                                               VertexBuilder builder)
    {
        final int startX = center.getX() - radius;
        final int startY = center.getY() - radius;
        final int startZ = center.getZ() - radius;
        final int endX = center.getX() + radius;
        final int endY = center.getY() + radius;
        final int endZ = center.getZ() + radius;
        int lastCX = startX >> 4;
        int lastCZ = startZ >> 4;
        Chunk chunk = world.getChunk(lastCX, lastCZ);
        BlockPos.MutBlockPos posMutable = new BlockPos.MutBlockPos();
        BlockPos.MutBlockPos posMutable2 = new BlockPos.MutBlockPos();

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

                    if (chunk.getBlockState(posMutable).getMaterial() == Material.AIR)
                    {
                        for (Direction side : Direction.VERTICAL_DIRECTIONS)
                        {
                            posMutable2.set(posMutable.getX() + side.getXOffset(),
                                            posMutable.getY() + side.getYOffset(),
                                            posMutable.getZ() + side.getZOffset());

                            if (chunk.getBlockState(posMutable2).getMaterial() != Material.AIR)
                            {
                                ShapeRenderUtils.renderBlockPosEdgeLines(posMutable, 0.001, color, cameraPos, builder);
                                break;
                            }
                        }

                        for (Direction side : Direction.HORIZONTAL_DIRECTIONS)
                        {
                            posMutable2.set(posMutable.getX() + side.getXOffset(),
                                            posMutable.getY() + side.getYOffset(),
                                            posMutable.getZ() + side.getZOffset());

                            if (world.getBlockState(posMutable2).getMaterial() != Material.AIR)
                            {
                                ShapeRenderUtils.renderBlockPosEdgeLines(posMutable, 0.001, color, cameraPos, builder);
                                break;
                            }
                        }
                    }
                }
            }
        }
    }
}
