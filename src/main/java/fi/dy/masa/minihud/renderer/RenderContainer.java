package fi.dy.masa.minihud.renderer;

import fi.dy.masa.malilib.render.overlay.BaseOverlayRenderer;
import fi.dy.masa.malilib.render.overlay.OverlayRendererContainer;
import fi.dy.masa.minihud.config.RendererToggle;

public class RenderContainer
{
    public static final OverlayRendererBeaconRange BEACON_OVERLAY                       = register(new OverlayRendererBeaconRange());
    public static final OverlayRendererBlockGrid BLOCK_GRID_OVERLAY                     = register(new OverlayRendererBlockGrid());
    public static final OverlayRendererLightLevel LIGHT_LEVEL_OVERLAY                   = register(new OverlayRendererLightLevel());
    public static final OverlayRendererRandomTickableChunks RANDOM_TICKS_FIXED_OVERLAY  = register(new OverlayRendererRandomTickableChunks(RendererToggle.OVERLAY_RANDOM_TICKS_FIXED));
    public static final OverlayRendererRandomTickableChunks RANDOM_TICKS_PLAYER_OVERLAY = register(new OverlayRendererRandomTickableChunks(RendererToggle.OVERLAY_RANDOM_TICKS_PLAYER));
    public static final OverlayRendererRegion REGION_FILES_OVERLAY                      = register(new OverlayRendererRegion());
    public static final OverlayRendererSlimeChunks SLIME_CHUNKS_OVERLAY                 = register(new OverlayRendererSlimeChunks());
    public static final OverlayRendererSpawnableChunks SPAWNABLE_CHUNKS_FIXED_OVERLAY   = register(new OverlayRendererSpawnableChunks(RendererToggle.OVERLAY_SPAWNABLE_CHUNKS_FIXED));
    public static final OverlayRendererSpawnableChunks SPAWNABLE_CHUNKS_PLAYER_OVERLAY  = register(new OverlayRendererSpawnableChunks(RendererToggle.OVERLAY_SPAWNABLE_CHUNKS_PLAYER));
    public static final OverlayRendererSpawnableColumnHeights SPAWNABLE_COLUMNS_OVERLAY = register(new OverlayRendererSpawnableColumnHeights());
    public static final OverlayRendererSpawnChunks SPAWN_CHUNKS_PREVIEW_OVERLAY         = register(new OverlayRendererSpawnChunks(RendererToggle.OVERLAY_SPAWN_CHUNK_OVERLAY_PLAYER));
    public static final OverlayRendererSpawnChunks SPAWN_CHUNKS_REAL_OVERLAY            = register(new OverlayRendererSpawnChunks(RendererToggle.OVERLAY_SPAWN_CHUNK_OVERLAY_REAL));
    public static final OverlayRendererSpawnerPositions SPAWNER_RENDERER                = register(new OverlayRendererSpawnerPositions());
    public static final OverlayRendererStructures STRUCTURE_BOUNDING_BOXES_OVERLAY      = register(new OverlayRendererStructures());
    public static final OverlayRendererWaterFalls WATER_FALL_RENDERER                   = register(new OverlayRendererWaterFalls());

    private static <T extends BaseOverlayRenderer> T register(T renderer)
    {
        OverlayRendererContainer.INSTANCE.addRenderer(renderer);
        return renderer;
    }
}
