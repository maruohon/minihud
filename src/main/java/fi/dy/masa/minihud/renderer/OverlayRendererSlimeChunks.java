package fi.dy.masa.minihud.renderer;

import com.google.gson.JsonObject;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.vertex.DefaultVertexFormats;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.BlockPos.PooledMutableBlockPos;
import net.minecraft.util.math.MathHelper;
import net.minecraft.util.math.Vec3d;
import fi.dy.masa.malilib.render.ShapeRenderUtils;
import fi.dy.masa.malilib.render.overlay.BaseRenderObject;
import fi.dy.masa.malilib.util.EntityUtils;
import fi.dy.masa.malilib.util.GameUtils;
import fi.dy.masa.malilib.util.JsonUtils;
import fi.dy.masa.malilib.util.WorldUtils;
import fi.dy.masa.malilib.util.data.Color4f;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.data.DataStorage;
import fi.dy.masa.minihud.util.MiscUtils;

public class OverlayRendererSlimeChunks extends MiniHUDOverlayRenderer
{
    protected boolean wasSeedKnown;
    protected long seed;
    protected double topY;

    @Override
    public void onEnabled()
    {
        if (this.shouldRender(GameUtils.getClient()))
        {
            Vec3d pos = EntityUtils.getCameraEntityPosition();
            this.topY = pos.y;

            this.setNeedsUpdate();
        }
    }

    @Override
    public boolean shouldRender(Minecraft mc)
    {
        return RendererToggle.OVERLAY_SLIME_CHUNKS_OVERLAY.isRendererEnabled() &&
                DataStorage.getInstance().isWorldSeedKnown(WorldUtils.getDimensionId(mc.world)) &&
                mc.world.provider.isSurfaceWorld();
    }

    @Override
    public boolean needsUpdate(Entity entity, Minecraft mc)
    {
        if (this.needsUpdate)
        {
            return true;
        }

        boolean isSeedKnown = DataStorage.getInstance().isWorldSeedKnown(entity.dimension);
        long seed = DataStorage.getInstance().getWorldSeed(entity.dimension);

        if (this.topY != Configs.Internal.SLIME_CHUNKS_OVERLAY_TOP_Y.getDoubleValue() ||
            this.wasSeedKnown != isSeedKnown || this.seed != seed)
        {
            return true;
        }

        int ex = (int) Math.floor(entity.posX);
        int ez = (int) Math.floor(entity.posZ);
        int lx = this.lastUpdatePos.getX();
        int lz = this.lastUpdatePos.getZ();

        return Math.abs(lx - ex) > 16 || Math.abs(lz - ez) > 16;
    }

    @Override
    public void update(Vec3d cameraPos, Entity entity, Minecraft mc)
    {
        DataStorage data = DataStorage.getInstance();
        this.topY = Configs.Internal.SLIME_CHUNKS_OVERLAY_TOP_Y.getDoubleValue();
        this.wasSeedKnown = data.isWorldSeedKnown(entity.dimension);
        this.seed = data.getWorldSeed(entity.dimension);

        if (this.wasSeedKnown)
        {
            final int centerX = MathHelper.floor(entity.posX) >> 4;
            final int centerZ = MathHelper.floor(entity.posZ) >> 4;
            final Color4f colorLines = Configs.Colors.SLIME_CHUNKS_OVERLAY_COLOR.getColor();
            final Color4f colorSides = Color4f.fromColor(colorLines, colorLines.a / 6);
            PooledMutableBlockPos pos1 = PooledMutableBlockPos.retain();
            PooledMutableBlockPos pos2 = PooledMutableBlockPos.retain();
            int r = MathHelper.clamp(Configs.Generic.SLIME_CHUNK_OVERLAY_RADIUS.getIntegerValue(), -1, 40);

            if (r == -1)
            {
                r = GameUtils.getRenderDistanceChunks();
            }

            BaseRenderObject renderQuads = this.renderObjects.get(0);
            BaseRenderObject renderLines = this.renderObjects.get(1);
            BUFFER_1.begin(renderQuads.getGlMode(), DefaultVertexFormats.POSITION_COLOR);
            BUFFER_2.begin(renderLines.getGlMode(), DefaultVertexFormats.POSITION_COLOR);
            int topY = (int) Math.floor(this.topY);

            for (int xOff = -r; xOff <= r; xOff++)
            {
                for (int zOff = -r; zOff <= r; zOff++)
                {
                    int cx = centerX + xOff;
                    int cz = centerZ + zOff;

                    if (MiscUtils.canSlimeSpawnInChunk(cx, cz, this.seed))
                    {
                        pos1.setPos( cx << 4,          0,  cz << 4);
                        pos2.setPos((cx << 4) + 15, topY, (cz << 4) + 15);
                        ShapeRenderUtils.renderBoxSidesAndEdges(pos1, pos2, colorLines, colorSides, BUFFER_1, BUFFER_2, cameraPos);
                    }
                }
            }

            pos1.release();
            pos2.release();

            BUFFER_1.finishDrawing();
            BUFFER_2.finishDrawing();

            renderQuads.uploadData(BUFFER_1);
            renderLines.uploadData(BUFFER_2);
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
