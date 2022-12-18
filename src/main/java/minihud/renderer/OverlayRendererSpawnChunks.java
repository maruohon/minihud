package minihud.renderer;

import org.apache.commons.lang3.tuple.Pair;

import net.minecraft.client.renderer.vertex.DefaultVertexFormats;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Vec3d;
import net.minecraft.world.World;

import malilib.render.ShapeRenderUtils;
import malilib.render.overlay.BaseRenderObject;
import malilib.util.data.Color4f;
import malilib.util.game.wrap.EntityWrap;
import malilib.util.game.wrap.GameUtils;
import minihud.config.Configs;
import minihud.config.RendererToggle;
import minihud.data.DataStorage;

public class OverlayRendererSpawnChunks extends MiniHudOverlayRenderer
{
    protected final RendererToggle toggle;

    public OverlayRendererSpawnChunks(RendererToggle toggle)
    {
        this.toggle = toggle;
    }

    @Override
    public boolean shouldRender()
    {
        World world = GameUtils.getClientWorld();

        return this.toggle.isRendererEnabled() &&
                (this.toggle == RendererToggle.SPAWN_CHUNKS_PLAYER ||
                 (world != null && world.provider.isSurfaceWorld() &&
                  DataStorage.getInstance().isWorldSpawnKnown()));
    }

    @Override
    public boolean needsUpdate(Entity entity)
    {
        if (this.needsUpdate)
        {
            return true;
        }

        int ex = (int) Math.floor(EntityWrap.getX(entity));
        int ez = (int) Math.floor(EntityWrap.getZ(entity));
        int lx = this.lastUpdatePos.getX();
        int lz = this.lastUpdatePos.getZ();

        // Player-following renderer
        if (this.toggle == RendererToggle.SPAWN_CHUNKS_PLAYER)
        {
            return ex != lx || ez != lz;
        }

        return Math.abs(lx - ex) > 16 || Math.abs(lz - ez) > 16;
    }

    @Override
    public void update(Vec3d cameraPos, Entity entity)
    {
        DataStorage data = DataStorage.getInstance();
        BlockPos spawn = this.toggle == RendererToggle.SPAWN_CHUNKS_PLAYER ? EntityWrap.getEntityBlockPos(entity) : data.getWorldSpawn();

        BaseRenderObject renderQuads = this.renderObjects.get(0);
        BaseRenderObject renderLines = this.renderObjects.get(1);
        BUFFER_1.begin(renderQuads.getGlMode(), DefaultVertexFormats.POSITION_COLOR);
        BUFFER_2.begin(renderLines.getGlMode(), DefaultVertexFormats.POSITION_COLOR);

        final Color4f colorEntity = this.toggle == RendererToggle.SPAWN_CHUNKS_REAL ?
                                            Configs.Colors.SPAWN_REAL_ENTITY_OVERLAY_COLOR.getColor() :
                                            Configs.Colors.SPAWN_PLAYER_ENTITY_OVERLAY_COLOR.getColor();
        final Color4f colorLazy = this.toggle == RendererToggle.SPAWN_CHUNKS_REAL ?
                                          Configs.Colors.SPAWN_REAL_LAZY_OVERLAY_COLOR.getColor() :
                                          Configs.Colors.SPAWN_PLAYER_LAZY_OVERLAY_COLOR.getColor();

        ShapeRenderUtils.renderBlockPosEdgeLines(spawn, 0.001, colorEntity, BUFFER_2, cameraPos);
        ShapeRenderUtils.renderBlockPosSideQuads(spawn, 0.001, colorEntity, BUFFER_1, cameraPos);

        Pair<BlockPos, BlockPos> corners = this.getSpawnChunkCorners(spawn, 128);
        RenderUtils.renderWallsWithLines(corners.getLeft(), corners.getRight(), cameraPos, 16, 16, true, colorLazy, BUFFER_1, BUFFER_2);

        corners = this.getSpawnChunkCorners(spawn, 128 - 32);
        RenderUtils.renderWallsWithLines(corners.getLeft(), corners.getRight(), cameraPos, 16, 16, true, colorEntity, BUFFER_1, BUFFER_2);

        BUFFER_1.finishDrawing();
        BUFFER_2.finishDrawing();

        renderQuads.uploadData(BUFFER_1);
        renderLines.uploadData(BUFFER_2);

        this.needsUpdate = false;
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
