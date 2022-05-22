package fi.dy.masa.minihud.renderer;

import java.util.HashSet;
import java.util.Set;
import javax.annotation.Nullable;
import com.google.gson.JsonObject;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.vertex.DefaultVertexFormats;
import net.minecraft.entity.Entity;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.ChunkPos;
import net.minecraft.util.math.Vec3d;
import fi.dy.masa.malilib.render.overlay.BaseRenderObject;
import fi.dy.masa.malilib.util.EntityUtils;
import fi.dy.masa.malilib.util.GameUtils;
import fi.dy.masa.malilib.util.PositionUtils;
import fi.dy.masa.malilib.util.data.Color4f;
import fi.dy.masa.malilib.util.data.json.JsonUtils;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.RendererToggle;

public class OverlayRendererRandomTickableChunks extends MiniHUDOverlayRenderer
{
    protected final RendererToggle toggle;
    protected Vec3d pos = Vec3d.ZERO;
    @Nullable protected Vec3d newPos;

    public OverlayRendererRandomTickableChunks(RendererToggle toggle)
    {
        this.toggle = toggle;
    }

    @Override
    public void onEnabled()
    {
        super.onEnabled();

        if (this.toggle == RendererToggle.RANDOM_TICKS_FIXED &&
            this.shouldRender(GameUtils.getClient()))
        {
            Vec3d pos = EntityUtils.getCameraEntityPosition();
            this.newPos = pos;
        }
    }

    @Override
    public boolean shouldRender(Minecraft mc)
    {
        return this.toggle.isRendererEnabled();
    }

    @Override
    public boolean needsUpdate(Entity entity, Minecraft mc)
    {
        if (this.toggle == RendererToggle.RANDOM_TICKS_FIXED)
        {
            return this.newPos != null;
        }
        // Player-following renderer
        else if (this.toggle == RendererToggle.RANDOM_TICKS_PLAYER)
        {
            return EntityUtils.getX(entity) != this.pos.x ||
                   EntityUtils.getZ(entity) != this.pos.z;
        }

        return false;
    }

    @Override
    public void update(Vec3d cameraPos, Entity entity, Minecraft mc)
    {
        if (this.toggle == RendererToggle.RANDOM_TICKS_PLAYER)
        {
            this.pos = entity.getPositionVector();
        }
        else if (this.newPos != null)
        {
            this.pos = this.newPos;
            this.newPos = null;
        }

        final Color4f color = this.toggle == RendererToggle.RANDOM_TICKS_PLAYER ?
                Configs.Colors.RANDOM_TICKS_PLAYER_OVERLAY_COLOR.getColor() :
                Configs.Colors.RANDOM_TICKS_FIXED_OVERLAY_COLOR.getColor();

        BaseRenderObject renderQuads = this.renderObjects.get(0);
        BaseRenderObject renderLines = this.renderObjects.get(1);
        BUFFER_1.begin(renderQuads.getGlMode(), DefaultVertexFormats.POSITION_COLOR);
        BUFFER_2.begin(renderLines.getGlMode(), DefaultVertexFormats.POSITION_COLOR);

        Set<ChunkPos> chunks = this.getRandomTickableChunks(this.pos);

        for (ChunkPos pos : chunks)
        {
            this.renderChunkEdgesIfApplicable(pos, cameraPos, chunks, color);
        }

        BUFFER_1.finishDrawing();
        BUFFER_2.finishDrawing();

        renderQuads.uploadData(BUFFER_1);
        renderLines.uploadData(BUFFER_2);
    }

    protected Set<ChunkPos> getRandomTickableChunks(Vec3d posCenter)
    {
        Set<ChunkPos> set = new HashSet<>();
        final int centerChunkX = ((int) Math.floor(posCenter.x)) >> 4;
        final int centerChunkZ = ((int) Math.floor(posCenter.z)) >> 4;
        final double maxRange = 128D * 128D;
        final int r = 9;

        for (int cz = centerChunkZ - r; cz <= centerChunkZ + r; ++cz)
        {
            for (int cx = centerChunkX - r; cx <= centerChunkX + r; ++cx)
            {
                double dx = (double) (cx * 16 + 8) - posCenter.x;
                double dz = (double) (cz * 16 + 8) - posCenter.z;

                if ((dx * dx + dz * dz) < maxRange)
                {
                    set.add(new ChunkPos(cx, cz));
                }
            }
        }

        return set;
    }

    protected void renderChunkEdgesIfApplicable(ChunkPos pos, Vec3d cameraPos, Set<ChunkPos> chunks,Color4f color)
    {
        for (EnumFacing side : PositionUtils.HORIZONTAL_DIRECTIONS)
        {
            ChunkPos posTmp = new ChunkPos(pos.x + side.getXOffset(), pos.z + side.getZOffset());

            if (chunks.contains(posTmp) == false)
            {
                this.renderChunkEdge(pos, side, cameraPos, color);
            }
        }
    }

    private void renderChunkEdge(ChunkPos pos, EnumFacing side, Vec3d cameraPos, Color4f color)
    {
        double minX, minZ, maxX, maxZ;

        switch (side)
        {
            case NORTH:
                minX = (pos.x << 4);
                minZ = (pos.z << 4);
                maxX = (pos.x << 4) + 16.0;
                maxZ = (pos.z << 4);
                break;
            case SOUTH:
                minX = (pos.x << 4);
                minZ = (pos.z << 4) + 16.0;
                maxX = (pos.x << 4) + 16.0;
                maxZ = (pos.z << 4) + 16.0;
                break;
            case WEST:
                minX = (pos.x << 4);
                minZ = (pos.z << 4);
                maxX = (pos.x << 4);
                maxZ = (pos.z << 4) + 16.0;
                break;
            case EAST:
                minX = (pos.x << 4) + 16.0;
                minZ = (pos.z << 4);
                maxX = (pos.x << 4) + 16.0;
                maxZ = (pos.z << 4) + 16.0;
                break;
            default:
                return;
        }

        RenderUtils.renderWallWithLines(minX, 0, minZ, maxX, 256, maxZ, 16, 16, true, cameraPos, color, BUFFER_1, BUFFER_2);
    }

    @Override
    public String getSaveId()
    {
        return this.toggle == RendererToggle.RANDOM_TICKS_FIXED ? "random_tickable_chunks" : "";
    }

    @Nullable
    @Override
    public JsonObject toJson()
    {
        JsonObject obj = super.toJson();
        obj.add("pos", JsonUtils.vec3dToJson(this.pos));
        return obj;
    }

    @Override
    public void fromJson(JsonObject obj)
    {
        super.fromJson(obj);

        Vec3d pos = JsonUtils.vec3dFromJson(obj, "pos");

        if (pos != null && this.toggle == RendererToggle.RANDOM_TICKS_FIXED)
        {
            this.newPos = pos;
        }
    }
}
