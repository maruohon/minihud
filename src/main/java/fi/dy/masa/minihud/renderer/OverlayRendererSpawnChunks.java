package fi.dy.masa.minihud.renderer;

import org.apache.commons.lang3.tuple.Pair;
import net.minecraft.client.MinecraftClient;
import net.minecraft.client.render.BufferBuilder;
import net.minecraft.client.render.VertexFormats;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Vec3d;
import net.minecraft.world.World;
import fi.dy.masa.malilib.util.Color4f;
import fi.dy.masa.malilib.util.PositionUtils;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.util.DataStorage;
import fi.dy.masa.minihud.util.MiscUtils;

public class OverlayRendererSpawnChunks extends OverlayRendererBase
{
    protected static boolean needsUpdate = true;

    protected final RendererToggle toggle;
    protected final boolean isPlayerFollowing;

    public static void setNeedsUpdate()
    {
        needsUpdate = true;
    }

    public OverlayRendererSpawnChunks(RendererToggle toggle)
    {
        this.toggle = toggle;
        this.isPlayerFollowing = toggle == RendererToggle.OVERLAY_SPAWN_CHUNK_OVERLAY_PLAYER;
    }

    @Override
    public boolean shouldRender(MinecraftClient mc)
    {
        return this.toggle.getBooleanValue() &&
                (this.isPlayerFollowing ||
                 (mc.world != null && MiscUtils.isOverworld(mc.world) &&
                  DataStorage.getInstance().isWorldSpawnKnown()));
    }

    @Override
    public boolean needsUpdate(Entity entity, MinecraftClient mc)
    {
        if (needsUpdate)
        {
            return true;
        }

        // Use the client player, to allow looking from the camera perspective
        entity = this.isPlayerFollowing ? mc.player : entity;

        int ex = (int) Math.floor(entity.getX());
        int ez = (int) Math.floor(entity.getZ());
        int lx = this.lastUpdatePos.getX();
        int lz = this.lastUpdatePos.getZ();

        if (this.isPlayerFollowing)
        {
            return ex != lx || ez != lz;
        }

        int range = mc.options.viewDistance * 16;

        return Math.abs(lx - ex) > range || Math.abs(lz - ez) > range;
    }

    @Override
    public void update(Vec3d cameraPos, Entity entity, MinecraftClient mc)
    {
        // Use the client player, to allow looking from the camera perspective
        entity = this.isPlayerFollowing ? mc.player : entity;

        DataStorage data = DataStorage.getInstance();
        BlockPos spawn = this.isPlayerFollowing ? PositionUtils.getEntityBlockPos(entity) : data.getWorldSpawn();

        RenderObjectBase renderQuads = this.renderObjects.get(0);
        RenderObjectBase renderLines = this.renderObjects.get(1);
        BUFFER_1.begin(renderQuads.getGlMode(), VertexFormats.POSITION_COLOR);
        BUFFER_2.begin(renderLines.getGlMode(), VertexFormats.POSITION_COLOR);

        final Color4f colorEntity = this.isPlayerFollowing ?
                Configs.Colors.SPAWN_PLAYER_ENTITY_OVERLAY_COLOR.getColor() :
                Configs.Colors.SPAWN_REAL_ENTITY_OVERLAY_COLOR.getColor();
        final Color4f colorLazy = this.isPlayerFollowing ?
                Configs.Colors.SPAWN_PLAYER_LAZY_OVERLAY_COLOR.getColor() :
                Configs.Colors.SPAWN_REAL_LAZY_OVERLAY_COLOR.getColor();
        final Color4f colorOuter = this.isPlayerFollowing ?
                Configs.Colors.SPAWN_PLAYER_OUTER_OVERLAY_COLOR.getColor() :
                Configs.Colors.SPAWN_REAL_OUTER_OVERLAY_COLOR.getColor();

        fi.dy.masa.malilib.render.RenderUtils.drawBlockBoundingBoxOutlinesBatchedLines(spawn, cameraPos, colorEntity, 0.001, BUFFER_2);
        drawBlockBoundingBoxSidesBatchedQuads(spawn, cameraPos, colorEntity, 0.001, BUFFER_1);

        Pair<BlockPos, BlockPos> corners = this.getSpawnChunkCorners(spawn, 22, mc.world);
        RenderUtils.renderWallsWithLines(corners.getLeft(), corners.getRight(), cameraPos, 16, 16, true, colorOuter, BUFFER_1, BUFFER_2);

        corners = this.getSpawnChunkCorners(spawn, 11, mc.world);
        RenderUtils.renderWallsWithLines(corners.getLeft(), corners.getRight(), cameraPos, 16, 16, true, colorLazy, BUFFER_1, BUFFER_2);

        corners = this.getSpawnChunkCorners(spawn, 9, mc.world);
        RenderUtils.renderWallsWithLines(corners.getLeft(), corners.getRight(), cameraPos, 16, 16, true, colorEntity, BUFFER_1, BUFFER_2);

        BUFFER_1.end();
        BUFFER_2.end();

        renderQuads.uploadData(BUFFER_1);
        renderLines.uploadData(BUFFER_2);

        needsUpdate = false;
    }

    protected Pair<BlockPos, BlockPos> getSpawnChunkCorners(BlockPos worldSpawn, int chunkRange, World world)
    {
        int cx = (worldSpawn.getX() >> 4);
        int cz = (worldSpawn.getZ() >> 4);

        int minY = world != null ? world.getBottomY() : -64;
        int maxY = world != null ? world.getTopY() : 320;
        BlockPos pos1 = new BlockPos( (cx - chunkRange) << 4      , minY,  (cz - chunkRange) << 4);
        BlockPos pos2 = new BlockPos(((cx + chunkRange) << 4) + 15, maxY, ((cz + chunkRange) << 4) + 15);

        return Pair.of(pos1, pos2);
    }

    /**
     * Assumes a BufferBuilder in GL_QUADS mode has been initialized
     */
    public static void drawBlockBoundingBoxSidesBatchedQuads(BlockPos pos, Vec3d cameraPos, Color4f color, double expand, BufferBuilder buffer)
    {
        double minX = pos.getX() - cameraPos.x - expand;
        double minY = pos.getY() - cameraPos.y - expand;
        double minZ = pos.getZ() - cameraPos.z - expand;
        double maxX = pos.getX() - cameraPos.x + expand + 1;
        double maxY = pos.getY() - cameraPos.y + expand + 1;
        double maxZ = pos.getZ() - cameraPos.z + expand + 1;

        fi.dy.masa.malilib.render.RenderUtils.drawBoxAllSidesBatchedQuads(minX, minY, minZ, maxX, maxY, maxZ, color, buffer);
    }
}
