package minihud.renderer;

import com.google.gson.JsonObject;

import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.vertex.DefaultVertexFormats;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.BlockPos.PooledMutableBlockPos;
import net.minecraft.util.math.MathHelper;
import net.minecraft.util.math.Vec3d;
import net.minecraft.world.World;

import malilib.render.ShapeRenderUtils;
import malilib.render.overlay.BaseRenderObject;
import malilib.util.data.Color4f;
import malilib.util.data.json.JsonUtils;
import malilib.util.game.wrap.EntityWrap;
import malilib.util.game.wrap.GameUtils;
import minihud.config.Configs;
import minihud.config.RendererToggle;
import minihud.data.DataStorage;
import minihud.util.MiscUtils;

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
            Vec3d pos = EntityWrap.getCameraEntityPosition();
            this.topY = pos.y;

            this.setNeedsUpdate();
        }
    }

    @Override
    public boolean shouldRender(Minecraft mc)
    {
        return RendererToggle.SLIME_CHUNKS.isRendererEnabled() &&
                DataStorage.getInstance().isWorldSeedKnown(mc.world) &&
                mc.world.provider.isSurfaceWorld();
    }

    @Override
    public boolean needsUpdate(Entity entity, Minecraft mc)
    {
        if (this.needsUpdate)
        {
            return true;
        }

        World world = entity.getEntityWorld();
        boolean isSeedKnown = DataStorage.getInstance().isWorldSeedKnown(world);
        long seed = DataStorage.getInstance().getWorldSeed(world);

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
    public void update(Vec3d cameraPos, Entity entity, Minecraft mc)
    {
        DataStorage data = DataStorage.getInstance();
        World world = entity.getEntityWorld();
        this.topY = Configs.Internal.SLIME_CHUNKS_OVERLAY_TOP_Y.getDoubleValue();
        this.wasSeedKnown = data.isWorldSeedKnown(world);
        this.seed = data.getWorldSeed(world);

        if (this.wasSeedKnown)
        {
            final int centerX = EntityWrap.getChunkX(entity);
            final int centerZ = EntityWrap.getChunkZ(entity);
            final Color4f colorLines = Configs.Colors.SLIME_CHUNKS_OVERLAY_COLOR.getColor();
            final Color4f colorSides = colorLines.withAlpha(colorLines.a / 6);
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
