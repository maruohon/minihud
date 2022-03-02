package fi.dy.masa.minihud.renderer;

import net.minecraft.block.entity.BlockEntity;
import net.minecraft.block.entity.BlockEntityType;
import net.minecraft.client.MinecraftClient;
import net.minecraft.client.render.VertexFormats;
import net.minecraft.client.world.ClientChunkManager;
import net.minecraft.client.world.ClientWorld;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Vec3d;
import net.minecraft.world.World;
import net.minecraft.world.chunk.ChunkStatus;
import net.minecraft.world.chunk.WorldChunk;
import fi.dy.masa.malilib.config.IConfigBoolean;
import it.unimi.dsi.fastutil.longs.LongIterator;
import it.unimi.dsi.fastutil.longs.LongOpenHashSet;

public abstract class BaseBlockRangeOverlay<T extends BlockEntity> extends OverlayRendererBase
{
    protected final IConfigBoolean renderToggleConfig;
    protected final LongOpenHashSet blockPositions = new LongOpenHashSet();
    protected final BlockEntityType<T> blockEntityType;
    protected final Class<T> blockEntityClass;
    protected boolean needsFullRebuild;
    protected boolean needsUpdate;
    protected int updateDistance = 48;

    protected BaseBlockRangeOverlay(IConfigBoolean renderToggleConfig,
                                    BlockEntityType<T> blockEntityType,
                                    Class<T> blockEntityClass)
    {
        this.renderToggleConfig = renderToggleConfig;
        this.blockEntityType = blockEntityType;
        this.blockEntityClass = blockEntityClass;
    }

    public void clear()
    {
        synchronized (this.blockPositions)
        {
            this.blockPositions.clear();
        }
    }

    public void setNeedsUpdate()
    {
        if (this.renderToggleConfig.getBooleanValue() == false)
        {
            this.clear();
            return;
        }

        this.needsUpdate = true;
        this.needsFullRebuild = true;
    }

    public void onBlockStatusChange(BlockPos pos)
    {
        if (this.renderToggleConfig.getBooleanValue())
        {
            synchronized (this.blockPositions)
            {
                this.blockPositions.add(pos.asLong());
                this.needsUpdate = true;
            }
        }
    }

    @Override
    public boolean shouldRender(MinecraftClient mc)
    {
        return this.renderToggleConfig.getBooleanValue();
    }

    @Override
    public boolean needsUpdate(Entity cameraEntity, MinecraftClient mc)
    {
        return this.needsUpdate || this.lastUpdatePos == null ||
               Math.abs(cameraEntity.getX() - this.lastUpdatePos.getX()) > this.updateDistance ||
               Math.abs(cameraEntity.getZ() - this.lastUpdatePos.getZ()) > this.updateDistance;
    }

    @Override
    public void update(Vec3d cameraPos, Entity entity, MinecraftClient mc)
    {
        this.startBuffers();

        synchronized (this.blockPositions)
        {
            if (this.needsFullRebuild)
            {
                this.blockPositions.clear();
                this.fetchAllTargetBlockEntityPositions(mc.world, entity.getBlockPos(), mc);
                this.needsFullRebuild = false;
            }

            this.renderBlockRanges(entity.getEntityWorld(), cameraPos, mc);
        }

        this.uploadBuffers();

        this.needsUpdate = false;
    }

    protected void startBuffers()
    {
        BUFFER_1.begin(this.renderObjects.get(0).getGlMode(), VertexFormats.POSITION_COLOR);
        BUFFER_2.begin(this.renderObjects.get(1).getGlMode(), VertexFormats.POSITION_COLOR);
    }

    protected void uploadBuffers()
    {
        BUFFER_1.end();
        BUFFER_2.end();
        this.renderObjects.get(0).uploadData(BUFFER_1);
        this.renderObjects.get(1).uploadData(BUFFER_2);
    }

    protected void fetchAllTargetBlockEntityPositions(ClientWorld world, BlockPos centerPos, MinecraftClient mc)
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
                        if (be.getType() == this.blockEntityType)
                        {
                            this.blockPositions.add(be.getPos().asLong());
                        }
                    }
                }
            }
        }
    }

    protected void renderBlockRanges(World world, Vec3d cameraPos, MinecraftClient mc)
    {
        LongIterator it = this.blockPositions.iterator();
        BlockPos.Mutable mutablePos = new BlockPos.Mutable();
        double max = (mc.options.viewDistance + 2) * 16;
        max = max * max;

        while (it.hasNext())
        {
            mutablePos.set(it.nextLong());
            BlockEntity be = world.getBlockEntity(mutablePos);

            if (be == null || this.blockEntityClass.isAssignableFrom(be.getClass()) == false)
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

            T castBe = this.blockEntityClass.cast(be);
            this.renderBlockRange(world, mutablePos, castBe, cameraPos);
        }
    }

    protected int getTopYOverTerrain(World world, BlockPos pos, int range)
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

    protected abstract void renderBlockRange(World world, BlockPos pos, T be, Vec3d cameraPos);
}
