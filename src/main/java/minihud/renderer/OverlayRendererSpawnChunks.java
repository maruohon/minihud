package minihud.renderer;

import org.apache.commons.lang3.tuple.Pair;

import net.minecraft.entity.Entity;
import net.minecraft.world.World;

import malilib.render.ShapeRenderUtils;
import malilib.util.data.Color4f;
import malilib.util.game.wrap.EntityWrap;
import malilib.util.game.wrap.GameWrap;
import malilib.util.position.BlockPos;
import malilib.util.position.Vec3d;
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
        World world = GameWrap.getClientWorld();

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

        final Color4f colorEntity = this.toggle == RendererToggle.SPAWN_CHUNKS_REAL ?
                                            Configs.Colors.SPAWN_REAL_ENTITY_OVERLAY_COLOR.getColor() :
                                            Configs.Colors.SPAWN_PLAYER_ENTITY_OVERLAY_COLOR.getColor();
        final Color4f colorLazy = this.toggle == RendererToggle.SPAWN_CHUNKS_REAL ?
                                          Configs.Colors.SPAWN_REAL_LAZY_OVERLAY_COLOR.getColor() :
                                          Configs.Colors.SPAWN_PLAYER_LAZY_OVERLAY_COLOR.getColor();

        this.startBuffers();

        ShapeRenderUtils.renderBlockPosSideQuads(spawn, 0.001, colorEntity, cameraPos, this.quadBuilder);
        ShapeRenderUtils.renderBlockPosEdgeLines(spawn, 0.001, colorEntity, cameraPos, this.lineBuilder);

        Pair<BlockPos, BlockPos> corners = this.getSpawnChunkCorners(spawn, 128);
        RenderUtils.renderWallsWithLines(corners.getLeft(), corners.getRight(), cameraPos, 16, 16,
                                         true, colorLazy, this.quadBuilder, this.lineBuilder);

        corners = this.getSpawnChunkCorners(spawn, 128 - 32);
        RenderUtils.renderWallsWithLines(corners.getLeft(), corners.getRight(), cameraPos, 16, 16,
                                         true, colorEntity, this.quadBuilder, this.lineBuilder);

        this.uploadBuffers();
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
