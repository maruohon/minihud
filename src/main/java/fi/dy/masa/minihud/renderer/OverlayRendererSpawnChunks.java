package fi.dy.masa.minihud.renderer;

import org.apache.commons.lang3.tuple.Pair;
import org.lwjgl.opengl.GL11;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.util.DataStorage;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.vertex.DefaultVertexFormats;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.BlockPos;

public class OverlayRendererSpawnChunks extends OverlayRendererBase
{
    protected final RendererToggle toggle;
    protected final int colorEntityProcessing;
    protected final int colorLazy;
    protected final BlockPos.MutableBlockPos posMutable = new BlockPos.MutableBlockPos();
    protected boolean rendered;

    public OverlayRendererSpawnChunks(RendererToggle toggle, int colorEntityProcessing, int colorLazy)
    {
        this.toggle = toggle;
        this.colorEntityProcessing = colorEntityProcessing;
        this.colorLazy = colorLazy;
    }

    @Override
    public boolean shouldRender(Minecraft mc)
    {
        return this.toggle.getBooleanValue() &&
                (this.toggle == RendererToggle.OVERLAY_SPAWN_CHUNK_OVERLAY_PLAYER ||
                 DataStorage.getInstance().isWorldSpawnKnown());
    }

    @Override
    public boolean needsUpdate(Entity entity, Minecraft mc)
    {
        if (this.rendered == false)
        {
            return true;
        }

        int ex = (int) Math.floor(entity.posX);
        int ez = (int) Math.floor(entity.posZ);
        int lx = this.lastUpdatePos.getX();
        int lz = this.lastUpdatePos.getZ();

        // Player-following renderer
        if (this.toggle == RendererToggle.OVERLAY_SPAWN_CHUNK_OVERLAY_PLAYER)
        {
            return ex != lx || ez != lz;
        }

        return Math.abs(lx - ex) > 16 || Math.abs(lz - ez) > 16;
    }

    @Override
    public void update(Entity entity, Minecraft mc)
    {
        DataStorage data = DataStorage.getInstance();
        BlockPos spawn = this.toggle == RendererToggle.OVERLAY_SPAWN_CHUNK_OVERLAY_PLAYER ? new BlockPos(entity) : data.getWorldSpawn();

        BUFFER_1.begin(GL11.GL_QUADS, DefaultVertexFormats.POSITION_COLOR);
        BUFFER_2.begin(GL11.GL_LINES, DefaultVertexFormats.POSITION_COLOR);

        int rangeH = (mc.gameSettings.renderDistanceChunks + 1) * 16;
        Pair<BlockPos, BlockPos> corners = this.getSpawnChunkCorners(spawn, 128);
        RenderUtils.renderVerticalWallsOfLinesWithinRange(BUFFER_1, BUFFER_2, corners.getLeft(), corners.getRight(),
                rangeH, 256, 16, 16, entity, this.colorLazy);

        corners = this.getSpawnChunkCorners(spawn, 128 - 32);
        RenderUtils.renderVerticalWallsOfLinesWithinRange(BUFFER_1, BUFFER_2, corners.getLeft(), corners.getRight(),
                rangeH, 256, 16, 16, entity, this.colorEntityProcessing);

        BUFFER_1.finishDrawing();
        BUFFER_2.finishDrawing();

        this.renderObjects.get(0).uploadData(BUFFER_1);
        this.renderObjects.get(1).uploadData(BUFFER_2);

        this.lastUpdatePos = new BlockPos(entity);
        this.rendered = true;
    }

    @Override
    public void allocateGlResources()
    {
        this.allocateBuffer(GL11.GL_QUADS);
        this.allocateBuffer(GL11.GL_LINES);
    }

    protected Pair<BlockPos, BlockPos> getSpawnChunkCorners(BlockPos worldSpawn, int spawnChunkRange)
    {
        int x;
        int z;
        x = (worldSpawn.getX() - (spawnChunkRange - 7)) & ~0xF;
        z = (worldSpawn.getZ() - (spawnChunkRange - 7)) & ~0xF;
        BlockPos pos1 = new BlockPos(x, 0, z);

        x = ((worldSpawn.getX() + (spawnChunkRange - 8)) & ~0xF) + 16 - 1;
        z = ((worldSpawn.getZ() + (spawnChunkRange - 8)) & ~0xF) + 16 - 1;
        BlockPos pos2 = new BlockPos(x, 256, z);

        return Pair.of(pos1, pos2);
    }
}
