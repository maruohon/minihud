package minihud.renderer;

import java.util.ArrayList;
import java.util.List;
import java.util.function.BooleanSupplier;
import java.util.function.Supplier;
import it.unimi.dsi.fastutil.longs.Long2ObjectOpenHashMap;

import net.minecraft.entity.Entity;

import malilib.render.RenderContext;
import malilib.render.ShapeRenderUtils;
import malilib.render.TextRenderUtils;
import malilib.util.data.Color4f;
import malilib.util.game.wrap.EntityWrap;
import malilib.util.position.BlockPos;
import malilib.util.position.ChunkPos;
import malilib.util.position.Vec3d;
import minihud.data.OrderedBlockPosLong;

public abstract class BaseBlockPositionListOverlayRenderer extends MiniHudOverlayRenderer
{
    protected final Long2ObjectOpenHashMap<ArrayList<OrderedBlockPosLong>> textPositions = new Long2ObjectOpenHashMap<>();
    protected final Supplier<Long2ObjectOpenHashMap<ArrayList<OrderedBlockPosLong>>> dataSource;
    protected final Supplier<Color4f> overlayColorSupplier;
    protected final BooleanSupplier enabledSupplier;
    protected final BooleanSupplier dirtySupplier;
    protected boolean wasDisabled = true;
    protected int updatePositionHysteresis = 16;
    protected int overlayRenderChunkRange = 8;
    protected int textRenderChunkRange = 2;

    public BaseBlockPositionListOverlayRenderer(BooleanSupplier enabledSupplier,
                                                Supplier<Color4f> overlayColorSupplier,
                                                BooleanSupplier dirtySupplier,
                                                Supplier<Long2ObjectOpenHashMap<ArrayList<OrderedBlockPosLong>>> dataSource)
    {
        this.enabledSupplier = enabledSupplier;
        this.dirtySupplier = dirtySupplier;
        this.overlayColorSupplier = overlayColorSupplier;
        this.dataSource = dataSource;
    }

    @Override
    public boolean needsUpdate(Entity entity)
    {
        int hysteresis = this.updatePositionHysteresis;

        return this.wasDisabled || this.dirtySupplier.getAsBoolean() ||
               Math.abs(EntityWrap.getX(entity) - this.lastUpdatePos.getX()) > hysteresis ||
               Math.abs(EntityWrap.getZ(entity) - this.lastUpdatePos.getZ()) > hysteresis;
    }

    @Override
    public void update(Vec3d cameraPos, Entity entity)
    {
        this.startBuffers();

        this.renderPositionsWithinChunkRange(this.lastUpdatePos,
                                             this.overlayRenderChunkRange,
                                             this.textRenderChunkRange, cameraPos);

        this.uploadBuffers();
        this.wasDisabled = false;
    }

    protected void renderPositionsWithinChunkRange(BlockPos center, int chunkRange, int textRenderChunkRange, Vec3d cameraPos)
    {
        Color4f colorQuads = this.overlayColorSupplier.get();
        Color4f colorLines = colorQuads.withAlpha(1.0f);
        Long2ObjectOpenHashMap<ArrayList<OrderedBlockPosLong>> map = this.dataSource.get();
        int centerChunkX = center.getX() >> 4;
        int centerChunkZ = center.getZ() >> 4;

        this.textPositions.clear();

        for (int cx = centerChunkX - chunkRange; cx <= centerChunkX + chunkRange; ++cx)
        {
            for (int cz = centerChunkZ - chunkRange; cz <= centerChunkZ + chunkRange; ++cz)
            {
                long cp = ChunkPos.asLong(cx, cz);
                ArrayList<OrderedBlockPosLong> positions = map.get(cp);

                if (positions != null)
                {
                    this.renderPositions(positions, cameraPos, colorQuads, colorLines);

                    if (Math.abs(cx - centerChunkX) <= textRenderChunkRange &&
                        Math.abs(cz - centerChunkZ) <= textRenderChunkRange)
                    {
                        ArrayList<OrderedBlockPosLong> list = this.textPositions.computeIfAbsent(cp, c -> new ArrayList<>());
                        list.addAll(positions);
                    }
                }
            }
        }
    }

    protected void renderPositions(List<OrderedBlockPosLong> positions, Vec3d cameraPos,
                                   Color4f colorQuads, Color4f colorLines)
    {
        for (OrderedBlockPosLong orderedPos : positions)
        {
            BlockPos pos = orderedPos.getPos();
            ShapeRenderUtils.renderBlockPosSideQuads(pos, 0.001, colorQuads, cameraPos, this.quadBuilder);
            ShapeRenderUtils.renderBlockPosEdgeLines(pos, 0.001, colorLines, cameraPos, this.lineBuilder);
        }
    }

    public void renderPositionText(double dx, double dy, double dz, RenderContext ctx)
    {
        ArrayList<String> list = new ArrayList<>();
        final float scale = 0.025f;

        for (ArrayList<OrderedBlockPosLong> posList : this.textPositions.values())
        {
            int count = posList.size();

            for (OrderedBlockPosLong orderedPos : posList)
            {
                BlockPos pos = orderedPos.getPos();
                int posX = pos.getX();
                int posY = pos.getY();
                int posZ = pos.getZ();

                list.add(String.format("%d, %d, %d", posX, posY, posZ));
                list.add(String.format("%d of %d", orderedPos.order, count));

                TextRenderUtils.renderTextPlate(list, posX + 0.5 - dx, posY + 1.75 - dy, posZ + 0.5 - dz, scale, ctx);
                list.clear();
            }
        }
    }
}
