package fi.dy.masa.minihud.renderer;

import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;
import net.minecraft.client.MinecraftClient;
import net.minecraft.client.render.VertexFormats;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.MathHelper;
import net.minecraft.util.math.Vec3d;
import net.minecraft.world.World;
import fi.dy.masa.malilib.util.Color4f;
import fi.dy.masa.malilib.util.JsonUtils;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.util.DataStorage;
import fi.dy.masa.minihud.util.MiscUtils;

public class OverlayRendererSlimeChunks extends OverlayRendererBase
{
    public static double overlayTopY;
    protected static boolean needsUpdate = true;

    protected boolean wasSeedKnown;
    protected long seed;
    protected double topY;

    public static void setNeedsUpdate()
    {
        needsUpdate = true;
    }

    @Override
    public boolean shouldRender(MinecraftClient mc)
    {
        return RendererToggle.OVERLAY_SLIME_CHUNKS_OVERLAY.getBooleanValue() && mc.world != null &&
                DataStorage.getInstance().isWorldSeedKnown(mc.world) &&
                MiscUtils.isOverworld(mc.world);
    }

    @Override
    public boolean needsUpdate(Entity entity, MinecraftClient mc)
    {
        if (needsUpdate)
        {
            return true;
        }

        World world = entity.getEntityWorld();
        boolean isSeedKnown = DataStorage.getInstance().isWorldSeedKnown(world);
        long seed = DataStorage.getInstance().getWorldSeed(world);

        if (this.topY != overlayTopY || this.wasSeedKnown != isSeedKnown || this.seed != seed)
        {
            return true;
        }

        int ex = (int) Math.floor(entity.getX());
        int ez = (int) Math.floor(entity.getZ());
        int lx = this.lastUpdatePos.getX();
        int lz = this.lastUpdatePos.getZ();

        return Math.abs(lx - ex) > 16 || Math.abs(lz - ez) > 16;
    }

    @Override
    public void update(Vec3d cameraPos, Entity entity, MinecraftClient mc)
    {
        DataStorage data = DataStorage.getInstance();
        World world = entity.getEntityWorld();
        this.topY = overlayTopY;
        this.wasSeedKnown = data.isWorldSeedKnown(world);
        this.seed = data.getWorldSeed(world);

        if (this.wasSeedKnown)
        {
            final int centerX = MathHelper.floor(entity.getX()) >> 4;
            final int centerZ = MathHelper.floor(entity.getZ()) >> 4;
            final Color4f colorSides = Configs.Colors.SLIME_CHUNKS_OVERLAY_COLOR.getColor();
            final Color4f colorLines = Color4f.fromColor(colorSides, 1.0F);
            BlockPos.Mutable pos1 = new BlockPos.Mutable();
            BlockPos.Mutable pos2 = new BlockPos.Mutable();
            int r = MathHelper.clamp(Configs.Generic.SLIME_CHUNK_OVERLAY_RADIUS.getIntegerValue(), -1, 40);

            if (r == -1)
            {
                r = mc.options.viewDistance;
            }

            RenderObjectBase renderQuads = this.renderObjects.get(0);
            RenderObjectBase renderLines = this.renderObjects.get(1);
            BUFFER_1.begin(renderQuads.getGlMode(), VertexFormats.POSITION_COLOR);
            BUFFER_2.begin(renderLines.getGlMode(), VertexFormats.POSITION_COLOR);
            int minY = world != null ? world.getBottomY() : -64;
            int topY = (int) Math.floor(this.topY);

            for (int xOff = -r; xOff <= r; xOff++)
            {
                for (int zOff = -r; zOff <= r; zOff++)
                {
                    int cx = centerX + xOff;
                    int cz = centerZ + zOff;

                    if (MiscUtils.canSlimeSpawnInChunk(cx, cz, this.seed))
                    {
                        pos1.set( cx << 4,       minY,  cz << 4      );
                        pos2.set((cx << 4) + 15, topY, (cz << 4) + 15);
                        fi.dy.masa.malilib.render.RenderUtils.drawBoxWithEdgesBatched(pos1, pos2, cameraPos, colorLines, colorSides, BUFFER_1, BUFFER_2);
                    }
                }
            }

            BUFFER_1.end();
            BUFFER_2.end();

            renderQuads.uploadData(BUFFER_1);
            renderLines.uploadData(BUFFER_2);
        }

        needsUpdate = false;
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
