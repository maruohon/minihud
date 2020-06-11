package fi.dy.masa.minihud.renderer;

import org.lwjgl.opengl.GL11;
import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.vertex.DefaultVertexFormats;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.BlockPos.PooledMutableBlockPos;
import net.minecraft.util.math.MathHelper;
import net.minecraft.util.math.Vec3d;
import fi.dy.masa.malilib.render.RenderObjectBase;
import fi.dy.masa.malilib.util.Color4f;
import fi.dy.masa.malilib.util.JsonUtils;
import fi.dy.masa.malilib.util.WorldUtils;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.data.DataStorage;
import fi.dy.masa.minihud.util.MiscUtils;

public class OverlayRendererSlimeChunks extends OverlayRendererBase
{
    public static double overlayTopY;

    protected boolean wasSeedKnown;
    protected long seed;
    protected double topY;

    @Override
    public boolean shouldRender(Minecraft mc)
    {
        return RendererToggle.OVERLAY_SLIME_CHUNKS_OVERLAY.getBooleanValue() &&
                DataStorage.getInstance().isWorldSeedKnown(WorldUtils.getDimensionId(mc.world)) &&
                mc.world.provider.isSurfaceWorld();
    }

    @Override
    public boolean needsUpdate(Entity entity, Minecraft mc)
    {
        boolean isSeedKnown = DataStorage.getInstance().isWorldSeedKnown(entity.dimension);
        long seed = DataStorage.getInstance().getWorldSeed(entity.dimension);

        if (this.topY != overlayTopY || this.wasSeedKnown != isSeedKnown || this.seed != seed)
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
        this.topY = overlayTopY;
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
                r = mc.gameSettings.renderDistanceChunks;
            }

            RenderObjectBase renderQuads = this.renderObjects.get(0);
            RenderObjectBase renderLines = this.renderObjects.get(1);
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
                        fi.dy.masa.malilib.render.RenderUtils.drawBoxWithEdgesBatched(pos1, pos2, cameraPos, colorLines, colorSides, BUFFER_1, BUFFER_2);
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
    public void allocateGlResources()
    {
        this.allocateBuffer(GL11.GL_QUADS);
        this.allocateBuffer(GL11.GL_LINES);
    }

    @Override
    public String getSaveId()
    {
        return "slime_chunks";
    }

    @Override
    public JsonObject toJson()
    {
        JsonObject obj = new JsonObject();
        obj.add("y_top", new JsonPrimitive(overlayTopY));
        return obj;
    }

    @Override
    public void fromJson(JsonObject obj)
    {
        overlayTopY = JsonUtils.getFloat(obj, "y_top");
    }
}
