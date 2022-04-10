package fi.dy.masa.minihud.renderer;

import java.util.HashSet;
import java.util.Set;
import net.minecraft.block.state.IBlockState;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.BufferBuilder;
import net.minecraft.client.renderer.vertex.DefaultVertexFormats;
import net.minecraft.entity.Entity;
import net.minecraft.init.Blocks;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.tileentity.TileEntityBeacon;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.ChunkPos;
import net.minecraft.util.math.Vec3d;
import net.minecraft.world.World;
import net.minecraft.world.chunk.Chunk;
import fi.dy.masa.malilib.render.ShapeRenderUtils;
import fi.dy.masa.malilib.render.overlay.BaseRenderObject;
import fi.dy.masa.malilib.util.data.Color4f;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.RendererToggle;

public class OverlayRendererBeaconRange extends MiniHUDOverlayRenderer
{
    private final Set<BlockPos> BEACON_POSITIONS = new HashSet<>();
    private final Set<ChunkPos> BEACON_CHUNKS = new HashSet<>();

    public void clear()
    {
        synchronized (this.BEACON_POSITIONS)
        {
            this.BEACON_CHUNKS.clear();
            this.BEACON_POSITIONS.clear();
        }
    }

    @Override
    public void setNeedsUpdate()
    {
        super.setNeedsUpdate();

        if (RendererToggle.BEACON_RANGE.isRendererEnabled() == false)
        {
            this.clear();
        }
    }

    public void checkNeedsUpdate(BlockPos pos, IBlockState state)
    {
        synchronized (this.BEACON_POSITIONS)
        {
            if (RendererToggle.BEACON_RANGE.isRendererEnabled() &&
                (state.getBlock() == Blocks.BEACON || this.BEACON_POSITIONS.contains(pos)))
            {
                this.setNeedsUpdate();
            }
        }
    }

    public void checkNeedsUpdate(ChunkPos chunkPos)
    {
        synchronized (this.BEACON_POSITIONS)
        {
            if (RendererToggle.BEACON_RANGE.isRendererEnabled() &&
                        this.BEACON_CHUNKS.contains(chunkPos))
            {
                this.setNeedsUpdate();
            }
        }
    }

    @Override
    public boolean shouldRender(Minecraft mc)
    {
        return RendererToggle.BEACON_RANGE.isRendererEnabled();
    }

    @Override
    public boolean needsUpdate(Entity entity, Minecraft mc)
    {
        return this.needsUpdate || this.lastUpdatePos == null;
    }

    @Override
    public void update(Vec3d cameraPos, Entity entity, Minecraft mc)
    {
        this.clear();

        BaseRenderObject renderQuads = this.renderObjects.get(0);
        BaseRenderObject renderLines = this.renderObjects.get(1);
        BUFFER_1.begin(renderQuads.getGlMode(), DefaultVertexFormats.POSITION_COLOR);
        BUFFER_2.begin(renderLines.getGlMode(), DefaultVertexFormats.POSITION_COLOR);

        synchronized (this.BEACON_POSITIONS)
        {
            this.renderBeaconRanges(entity.getEntityWorld(), cameraPos, BUFFER_1, BUFFER_2);
        }

        BUFFER_1.finishDrawing();
        BUFFER_2.finishDrawing();
        renderQuads.uploadData(BUFFER_1);
        renderLines.uploadData(BUFFER_2);

        this.needsUpdate = false;
    }

    public static Color4f getColorForLevel(int level)
    {
        switch (level)
        {
            case 1: return Configs.Colors.BEACON_RANGE_LVL1_OVERLAY_COLOR.getColor();
            case 2: return Configs.Colors.BEACON_RANGE_LVL2_OVERLAY_COLOR.getColor();
            case 3: return Configs.Colors.BEACON_RANGE_LVL3_OVERLAY_COLOR.getColor();
            default: return Configs.Colors.BEACON_RANGE_LVL4_OVERLAY_COLOR.getColor();
        }
    }

    protected void renderBeaconRanges(World world, Vec3d cameraPos, BufferBuilder bufferQuads, BufferBuilder bufferLines)
    {
        for (TileEntity be : world.loadedTileEntityList)
        {
            if (be instanceof TileEntityBeacon)
            {
                BlockPos pos = be.getPos();
                int level = ((TileEntityBeacon) be).getLevels();

                if (level >= 1 && level <= 4)
                {
                    this.renderBeaconBox(world, pos, cameraPos, level, getColorForLevel(level), bufferQuads, bufferLines);
                }
            }
        }
    }

    protected void renderBeaconBox(World world, BlockPos pos, Vec3d cameraPos, int level, Color4f color, BufferBuilder bufferQuads, BufferBuilder bufferLines)
    {
        double x = pos.getX();
        double y = pos.getY();
        double z = pos.getZ();

        int range = level * 10 + 10;
        double minX = x - range - cameraPos.x;
        double minY = y - range - cameraPos.y;
        double minZ = z - range - cameraPos.z;
        double maxX = x + range + 1 - cameraPos.x;
        double maxY = this.getMaxHeight(world, pos, range) - cameraPos.y;
        double maxZ = z + range + 1 -cameraPos.z;

        ShapeRenderUtils.renderBoxSideQuads(minX, minY, minZ, maxX, maxY, maxZ, color, bufferQuads);
        ShapeRenderUtils.renderBoxEdgeLines(minX, minY, minZ, maxX, maxY, maxZ, color.withAlpha(1f), bufferLines);

        this.BEACON_POSITIONS.add(pos);
        this.BEACON_CHUNKS.add(new ChunkPos(pos.getX() >> 4, pos.getZ() >> 4));
    }

    protected int getMaxHeight(World world, BlockPos pos, int range)
    {
        final int minX = pos.getX() - range;
        final int minZ = pos.getZ() - range;
        final int maxX = pos.getX() + range;
        final int maxZ = pos.getZ() + range;

        final int minCX = minX >> 4;
        final int minCZ = minZ >> 4;
        final int maxCX = maxX >> 4;
        final int maxCZ = maxZ >> 4;
        int maxY = 0;

        for (int cz = minCZ; cz <= maxCZ; ++cz)
        {
            for (int cx = minCX; cx <= maxCX; ++cx)
            {
                final int xMin = Math.max(minX,  cx << 4      );
                final int zMin = Math.max(minZ,  cz << 4      );
                final int xMax = Math.min(maxX, (cx << 4) + 15);
                final int zMax = Math.min(maxZ, (cz << 4) + 15);
                Chunk chunk = world.getChunk(cx, cz);

                for (int z = zMin; z <= zMax; ++z)
                {
                    for (int x = xMin; x <= xMax; ++x)
                    {
                        int height = chunk.getHeightValue(x & 0xF, z & 0xF);

                        if (height > maxY)
                        {
                            maxY = height;
                        }
                    }
                }
            }
        }

        return maxY + 4;
    }
}
