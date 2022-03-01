package fi.dy.masa.minihud.renderer;

import net.minecraft.block.entity.BeaconBlockEntity;
import net.minecraft.block.entity.BlockEntity;
import net.minecraft.block.entity.BlockEntityType;
import net.minecraft.client.MinecraftClient;
import net.minecraft.client.render.BufferBuilder;
import net.minecraft.client.render.VertexFormats;
import net.minecraft.client.world.ClientChunkManager;
import net.minecraft.client.world.ClientWorld;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Vec3d;
import net.minecraft.world.World;
import net.minecraft.world.chunk.ChunkStatus;
import net.minecraft.world.chunk.WorldChunk;
import fi.dy.masa.malilib.util.Color4f;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.mixin.IMixinBeaconBlockEntity;
import it.unimi.dsi.fastutil.longs.LongIterator;
import it.unimi.dsi.fastutil.longs.LongOpenHashSet;

public class OverlayRendererBeaconRange extends OverlayRendererBase
{
    public static final OverlayRendererBeaconRange INSTANCE = new OverlayRendererBeaconRange();

    private final LongOpenHashSet beaconPositions = new LongOpenHashSet();
    private boolean needsFullRebuild;
    private boolean needsUpdate;

    public void clear()
    {
        synchronized (this.beaconPositions)
        {
            this.beaconPositions.clear();
        }
    }

    public void setNeedsUpdate()
    {
        if (RendererToggle.OVERLAY_BEACON_RANGE.getBooleanValue() == false)
        {
            this.clear();
            return;
        }

        this.needsUpdate = true;
        this.needsFullRebuild = true;
    }

    public void onBeaconLevelChange(BlockPos pos)
    {
        if (RendererToggle.OVERLAY_BEACON_RANGE.getBooleanValue())
        {
            synchronized (this.beaconPositions)
            {
                this.beaconPositions.add(pos.asLong());
                this.needsUpdate = true;
            }
        }
    }

    @Override
    public boolean shouldRender(MinecraftClient mc)
    {
        return RendererToggle.OVERLAY_BEACON_RANGE.getBooleanValue();
    }

    @Override
    public boolean needsUpdate(Entity cameraEntity, MinecraftClient mc)
    {
        int updateDistance = 48;

        return this.needsUpdate || this.lastUpdatePos == null ||
               Math.abs(cameraEntity.getX() - this.lastUpdatePos.getX()) > updateDistance ||
               Math.abs(cameraEntity.getZ() - this.lastUpdatePos.getZ()) > updateDistance;
    }

    @Override
    public void update(Vec3d cameraPos, Entity entity, MinecraftClient mc)
    {
        RenderObjectBase renderQuads = this.renderObjects.get(0);
        RenderObjectBase renderLines = this.renderObjects.get(1);
        BUFFER_1.begin(renderQuads.getGlMode(), VertexFormats.POSITION_COLOR);
        BUFFER_2.begin(renderLines.getGlMode(), VertexFormats.POSITION_COLOR);

        synchronized (this.beaconPositions)
        {
            if (this.needsFullRebuild)
            {
                this.beaconPositions.clear();
                this.fetchAllBeacons(mc.world, entity.getBlockPos(), mc);
                this.needsFullRebuild = false;
            }

            this.renderBeaconRanges(entity.getEntityWorld(), cameraPos, mc, BUFFER_1, BUFFER_2);
        }

        BUFFER_1.end();
        BUFFER_2.end();
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

    protected void fetchAllBeacons(ClientWorld world, BlockPos centerPos, MinecraftClient mc)
    {
        ClientChunkManager chunkManager = world.getChunkManager();
        int centerCX = centerPos.getX() >> 4;
        int centerCZ = centerPos.getZ() >> 4;
        int chunkRadius = mc.options.viewDistance;

        for (int cz = centerCZ - chunkRadius; cz <= centerCZ + chunkRadius; ++cz)
        {
            for (int cx = centerCX - chunkRadius; cx <= centerCX + chunkRadius; ++cx)
            {
                WorldChunk chunk = chunkManager.getChunk(cx, cz, ChunkStatus.FULL, false);

                if (chunk != null)
                {
                    for (BlockEntity be : chunk.getBlockEntities().values())
                    {
                        if (be.getType() == BlockEntityType.BEACON)
                        {
                            this.beaconPositions.add(be.getPos().asLong());
                        }
                    }
                }
            }
        }
    }

    protected void renderBeaconRanges(World world, Vec3d cameraPos, MinecraftClient mc,
                                      BufferBuilder bufferQuads, BufferBuilder bufferLines)
    {
        LongIterator it = this.beaconPositions.iterator();
        BlockPos.Mutable mutablePos = new BlockPos.Mutable();
        double max = (mc.options.viewDistance + 2) * 16;
        max = max * max;

        while (it.hasNext())
        {
            mutablePos.set(it.nextLong());
            BlockEntity be = world.getBlockEntity(mutablePos);

            if ((be instanceof BeaconBlockEntity) == false)
            {
                it.remove();
                continue;
            }

            double distSq = (cameraPos.x - mutablePos.getX()) * (cameraPos.x - mutablePos.getX()) +
                            (cameraPos.z - mutablePos.getZ()) * (cameraPos.z - mutablePos.getZ());

            if (distSq > max)
            {
                continue;
            }

            int level = ((IMixinBeaconBlockEntity) be).minihud_getLevel();

            if (level >= 1 && level <= 4)
            {
                this.renderBeaconBox(world, mutablePos, level, cameraPos, getColorForLevel(level), bufferQuads, bufferLines);
            }
        }
    }

    protected void renderBeaconBox(World world, BlockPos pos, int level, Vec3d cameraPos, Color4f color,
                                   BufferBuilder bufferQuads, BufferBuilder bufferLines)
    {
        double x = pos.getX() - cameraPos.x;
        double y = pos.getY() - cameraPos.y;
        double z = pos.getZ() - cameraPos.z;

        int range = level * 10 + 10;
        double minX = x - range;
        double minY = y - range;
        double minZ = z - range;
        double maxX = x + range + 1;
        double maxY = this.getMaxHeight(world, pos, range);
        double maxZ = z + range + 1;

        fi.dy.masa.malilib.render.RenderUtils.drawBoxAllSidesBatchedQuads(minX, minY, minZ, maxX, maxY, maxZ, color, bufferQuads);
        fi.dy.masa.malilib.render.RenderUtils.drawBoxAllEdgesBatchedLines(minX, minY, minZ, maxX, maxY, maxZ, Color4f.fromColor(color, 1f), bufferLines);
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
                WorldChunk chunk = world.getChunk(cx, cz);
                int height = chunk.getHighestNonEmptySectionYOffset() + 15;

                if (height > maxY)
                {
                    maxY = height;
                }
            }
        }

        return maxY + 4;
    }
}
