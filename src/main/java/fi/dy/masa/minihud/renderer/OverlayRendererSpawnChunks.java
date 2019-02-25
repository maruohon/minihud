package fi.dy.masa.minihud.renderer;

import org.apache.commons.lang3.tuple.Pair;
import org.lwjgl.opengl.GL11;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.util.DataStorage;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.vertex.DefaultVertexFormats;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.BlockPos;

public class OverlayRendererSpawnChunks extends OverlayRendererBase
{
    protected final RendererToggle toggle;
    protected boolean rendered;

    public OverlayRendererSpawnChunks(RendererToggle toggle)
    {
        this.toggle = toggle;
    }

    @Override
    public boolean shouldRender(Minecraft mc)
    {
        if (this.toggle.getBooleanValue() == false)
        {
            // A cheap hack to get it to re-render after toggling off/on
            this.rendered = false;
        }

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

        RenderObjectBase renderQuads = this.renderObjects.get(0);
        RenderObjectBase renderLines = this.renderObjects.get(1);
        BUFFER_1.begin(renderQuads.getGlMode(), DefaultVertexFormats.POSITION_COLOR);
        BUFFER_2.begin(renderLines.getGlMode(), DefaultVertexFormats.POSITION_COLOR);

        final int colorEntity = this.toggle == RendererToggle.OVERLAY_SPAWN_CHUNK_OVERLAY_REAL ?
                Configs.Colors.SPAWN_REAL_ENTITY_OVERLAY_COLOR.getIntegerValue() :
                Configs.Colors.SPAWN_PLAYER_ENTITY_OVERLAY_COLOR.getIntegerValue();
        final int colorLazy = this.toggle == RendererToggle.OVERLAY_SPAWN_CHUNK_OVERLAY_REAL ?
                Configs.Colors.SPAWN_REAL_LAZY_OVERLAY_COLOR.getIntegerValue() :
                Configs.Colors.SPAWN_PLAYER_LAZY_OVERLAY_COLOR.getIntegerValue();

        int rangeH = (mc.gameSettings.renderDistanceChunks + 1) * 16;
        Pair<BlockPos, BlockPos> corners = this.getSpawnChunkCorners(spawn, 128);
        RenderUtils.renderVerticalWallsOfLinesWithinRange(BUFFER_1, BUFFER_2, corners.getLeft(), corners.getRight(),
                rangeH, 256, 16, 16, entity, colorLazy);

        corners = this.getSpawnChunkCorners(spawn, 128 - 32);
        RenderUtils.renderVerticalWallsOfLinesWithinRange(BUFFER_1, BUFFER_2, corners.getLeft(), corners.getRight(),
                rangeH, 256, 16, 16, entity, colorEntity);

        BUFFER_1.finishDrawing();
        BUFFER_2.finishDrawing();

        renderQuads.uploadData(BUFFER_1);
        renderLines.uploadData(BUFFER_2);

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
