package minihud.renderer;

import com.google.gson.JsonObject;

import net.minecraft.entity.Entity;
import net.minecraft.world.World;

import malilib.render.ShapeRenderUtils;
import malilib.util.data.Color4f;
import malilib.util.data.json.JsonUtils;
import malilib.util.game.wrap.EntityWrap;
import malilib.util.game.wrap.GameUtils;
import malilib.util.position.BlockPos;
import malilib.util.position.Vec3d;
import minihud.config.Configs;
import minihud.config.RendererToggle;
import minihud.data.DataStorage;
import minihud.util.MiscUtils;

public class OverlayRendererSlimeChunks extends MiniHudOverlayRenderer
{
    protected boolean wasSeedKnown;
    protected long seed;
    protected double topY;

    @Override
    public void onEnabled()
    {
        if (this.shouldRender())
        {
            Vec3d pos = EntityWrap.getCameraEntityPosition();
            this.topY = pos.y;

            this.setNeedsUpdate();
        }
    }

    @Override
    public boolean shouldRender()
    {
        World world = GameUtils.getClientWorld();

        return RendererToggle.SLIME_CHUNKS.isRendererEnabled() &&
                DataStorage.INSTANCE.isWorldSeedKnown(world) &&
                world.provider.isSurfaceWorld();
    }

    @Override
    public boolean needsUpdate(Entity entity)
    {
        if (this.needsUpdate)
        {
            return true;
        }

        World world = GameUtils.getClientWorld();
        boolean isSeedKnown = DataStorage.INSTANCE.isWorldSeedKnown(world);
        long seed = DataStorage.INSTANCE.getWorldSeed(world);

        if (this.topY != Configs.Internal.SLIME_CHUNKS_OVERLAY_TOP_Y.getDoubleValue() ||
            this.wasSeedKnown != isSeedKnown || this.seed != seed)
        {
            return true;
        }

        int ex = (int) Math.floor(EntityWrap.getX(entity));
        int ez = (int) Math.floor(EntityWrap.getZ(entity));
        int lx = this.lastUpdatePos.getX();
        int lz = this.lastUpdatePos.getZ();

        return Math.abs(lx - ex) > 16 || Math.abs(lz - ez) > 16;
    }

    @Override
    public void update(Vec3d cameraPos, Entity entity)
    {
        DataStorage data = DataStorage.getInstance();
        World world = GameUtils.getClientWorld();
        this.topY = Configs.Internal.SLIME_CHUNKS_OVERLAY_TOP_Y.getDoubleValue();
        this.wasSeedKnown = data.isWorldSeedKnown(world);
        this.seed = data.getWorldSeed(world);

        if (this.wasSeedKnown)
        {
            final int centerX = EntityWrap.getChunkX(entity);
            final int centerZ = EntityWrap.getChunkZ(entity);
            final Color4f colorLines = Configs.Colors.SLIME_CHUNKS_OVERLAY_COLOR.getColor();
            final Color4f colorSides = colorLines.withAlpha(colorLines.a / 6);
            int r = Configs.Generic.SLIME_CHUNK_OVERLAY_RADIUS.getIntegerValue();
            BlockPos.MutBlockPos pos1 = new BlockPos.MutBlockPos();
            BlockPos.MutBlockPos pos2 = new BlockPos.MutBlockPos();

            if (r == -1)
            {
                r = GameUtils.getRenderDistanceChunks();
            }

            int topY = (int) Math.floor(this.topY);

            this.startBuffers();

            for (int xOff = -r; xOff <= r; xOff++)
            {
                for (int zOff = -r; zOff <= r; zOff++)
                {
                    int cx = centerX + xOff;
                    int cz = centerZ + zOff;

                    if (MiscUtils.canSlimeSpawnInChunk(cx, cz, this.seed))
                    {
                        pos1.set( cx << 4,          0,  cz << 4);
                        pos2.set((cx << 4) + 15, topY, (cz << 4) + 15);
                        ShapeRenderUtils.renderBoxSidesAndEdges(pos1, pos2, colorLines, colorSides, cameraPos,
                                                                this.quadBuilder, this.lineBuilder);
                    }
                }
            }

            this.uploadBuffers();
        }
    }

    @Override
    public String getSaveId()
    {
        return "slime_chunks";
    }

    @Override
    public JsonObject toJson()
    {
        JsonObject obj = super.toJson();
        obj.addProperty("top_y", this.topY);
        return obj;
    }

    @Override
    public void fromJson(JsonObject obj)
    {
        super.fromJson(obj);

        this.topY = JsonUtils.getDoubleOrDefault(obj, "top_y", 80);
    }
}
