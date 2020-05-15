package fi.dy.masa.minihud.renderer;

import org.apache.commons.lang3.tuple.Pair;
import org.lwjgl.opengl.GL11;
import net.minecraft.client.MinecraftClient;
import net.minecraft.client.render.VertexFormats;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Vec3d;
import net.minecraft.world.dimension.OverworldDimension;
import fi.dy.masa.malilib.util.Color4f;
import fi.dy.masa.malilib.util.PositionUtils;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.util.DataStorage;

public class OverlayRendererSpawnChunks extends OverlayRendererBase
{
    protected static boolean needsUpdate = true;

    protected final RendererToggle toggle;

    public static void setNeedsUpdate()
    {
        needsUpdate = true;
    }

    public OverlayRendererSpawnChunks(RendererToggle toggle)
    {
        this.toggle = toggle;
    }

    @Override
    public boolean shouldRender(MinecraftClient mc)
    {
        return this.toggle.getBooleanValue() &&
                (this.toggle == RendererToggle.OVERLAY_SPAWN_CHUNK_OVERLAY_PLAYER ||
                 (mc.world != null && mc.world.getDimension() instanceof OverworldDimension &&
                  DataStorage.getInstance().isWorldSpawnKnown()));
    }

    @Override
    public boolean needsUpdate(Entity entity, MinecraftClient mc)
    {
        if (needsUpdate)
        {
            return true;
        }

        int ex = (int) Math.floor(entity.getX());
        int ez = (int) Math.floor(entity.getZ());
        int lx = this.lastUpdatePos.getX();
        int lz = this.lastUpdatePos.getZ();

        // Player-following renderer
        if (this.toggle == RendererToggle.OVERLAY_SPAWN_CHUNK_OVERLAY_PLAYER)
        {
            return ex != lx || ez != lz;
        }

        int range = mc.options.viewDistance * 16;

        return Math.abs(lx - ex) > range || Math.abs(lz - ez) > range;
    }

    @Override
    public void update(Vec3d cameraPos, Entity entity, MinecraftClient mc)
    {
        DataStorage data = DataStorage.getInstance();
        BlockPos spawn = this.toggle == RendererToggle.OVERLAY_SPAWN_CHUNK_OVERLAY_PLAYER ? PositionUtils.getEntityBlockPos(entity) : data.getWorldSpawn();

        RenderObjectBase renderQuads = this.renderObjects.get(0);
        RenderObjectBase renderLines = this.renderObjects.get(1);
        BUFFER_1.begin(renderQuads.getGlMode(), VertexFormats.POSITION_COLOR);
        BUFFER_2.begin(renderLines.getGlMode(), VertexFormats.POSITION_COLOR);

        final Color4f colorEntity = this.toggle == RendererToggle.OVERLAY_SPAWN_CHUNK_OVERLAY_REAL ?
                Configs.Colors.SPAWN_REAL_ENTITY_OVERLAY_COLOR.getColor() :
                Configs.Colors.SPAWN_PLAYER_ENTITY_OVERLAY_COLOR.getColor();
        final Color4f colorLazy = this.toggle == RendererToggle.OVERLAY_SPAWN_CHUNK_OVERLAY_REAL ?
                Configs.Colors.SPAWN_REAL_LAZY_OVERLAY_COLOR.getColor() :
                Configs.Colors.SPAWN_PLAYER_LAZY_OVERLAY_COLOR.getColor();
        final Color4f colorOuter = this.toggle == RendererToggle.OVERLAY_SPAWN_CHUNK_OVERLAY_REAL ?
                Configs.Colors.SPAWN_REAL_OUTER_OVERLAY_COLOR.getColor() :
                Configs.Colors.SPAWN_PLAYER_OUTER_OVERLAY_COLOR.getColor();

        Pair<BlockPos, BlockPos> corners = this.getSpawnChunkCorners(spawn, 22);
        RenderUtils.renderWallsWithLines(corners.getLeft(), corners.getRight(), cameraPos, 16, 16, true, colorOuter, BUFFER_1, BUFFER_2);

        corners = this.getSpawnChunkCorners(spawn, 11);
        RenderUtils.renderWallsWithLines(corners.getLeft(), corners.getRight(), cameraPos, 16, 16, true, colorLazy, BUFFER_1, BUFFER_2);

        corners = this.getSpawnChunkCorners(spawn, 9);
        RenderUtils.renderWallsWithLines(corners.getLeft(), corners.getRight(), cameraPos, 16, 16, true, colorEntity, BUFFER_1, BUFFER_2);

        BUFFER_1.end();
        BUFFER_2.end();

        renderQuads.uploadData(BUFFER_1);
        renderLines.uploadData(BUFFER_2);

        needsUpdate = false;
    }

    @Override
    public void allocateGlResources()
    {
        this.allocateBuffer(GL11.GL_QUADS);
        this.allocateBuffer(GL11.GL_LINES);
    }

    protected Pair<BlockPos, BlockPos> getSpawnChunkCorners(BlockPos worldSpawn, int chunkRange)
    {
        int cx = (worldSpawn.getX() >> 4);
        int cz = (worldSpawn.getZ() >> 4);

        BlockPos pos1 = new BlockPos( (cx - chunkRange) << 4      ,   0,  (cz - chunkRange) << 4);
        BlockPos pos2 = new BlockPos(((cx + chunkRange) << 4) + 15, 256, ((cz + chunkRange) << 4) + 15);

        return Pair.of(pos1, pos2);
    }
}
