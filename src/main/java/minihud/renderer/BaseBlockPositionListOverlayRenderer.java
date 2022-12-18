package minihud.renderer;

import java.util.ArrayList;
import java.util.List;
import java.util.function.BooleanSupplier;
import java.util.function.Supplier;
import it.unimi.dsi.fastutil.longs.Long2ObjectOpenHashMap;
import org.lwjgl.opengl.GL11;

import net.minecraft.client.renderer.vertex.DefaultVertexFormats;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.ChunkPos;
import net.minecraft.util.math.Vec3d;

import malilib.render.ShapeRenderUtils;
import malilib.render.TextRenderUtils;
import malilib.render.overlay.BaseRenderObject;
import malilib.util.data.Color4f;
import malilib.util.game.wrap.EntityWrap;
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
        BaseRenderObject renderQuads = this.renderObjects.get(0);
        BaseRenderObject renderLines = this.renderObjects.get(1);
        BUFFER_1.begin(renderQuads.getGlMode(), DefaultVertexFormats.POSITION_COLOR);
        BUFFER_2.begin(renderLines.getGlMode(), DefaultVertexFormats.POSITION_COLOR);

        this.renderPositionsWithinChunkRange(this.lastUpdatePos,
                                             this.overlayRenderChunkRange,
                                             this.textRenderChunkRange, cameraPos);

        BUFFER_1.finishDrawing();
        BUFFER_2.finishDrawing();

        renderQuads.uploadData(BUFFER_1);
        renderLines.uploadData(BUFFER_2);

        this.wasDisabled = false;
    }

    @Override
    public void allocateGlResources()
    {
        this.allocateBuffer(GL11.GL_QUADS);
        this.allocateBuffer(GL11.GL_LINES);
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
                    this.renderPositions(positions, colorQuads, colorLines, cameraPos);

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

    protected void renderPositions(List<OrderedBlockPosLong> positions, Color4f colorQuads, Color4f colorLines, Vec3d cameraPos)
    {
        for (OrderedBlockPosLong orderedPos : positions)
        {
            BlockPos pos = orderedPos.getPos();
            ShapeRenderUtils.renderBlockPosSideQuads(pos, 0.001, colorQuads, BUFFER_1, cameraPos);
            ShapeRenderUtils.renderBlockPosEdgeLines(pos, 0.001, colorLines, BUFFER_2, cameraPos);
        }
    }

    public void renderPositionText(double dx, double dy, double dz)
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

                TextRenderUtils.renderTextPlate(list, posX + 0.5 - dx, posY + 1.75 - dy, posZ + 0.5 - dz, scale);
                list.clear();
            }
        }
    }
}
