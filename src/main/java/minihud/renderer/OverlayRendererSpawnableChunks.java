package minihud.renderer;

import javax.annotation.Nullable;
import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;

import net.minecraft.entity.Entity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Vec3d;

import malilib.util.data.Color4f;
import malilib.util.data.json.JsonUtils;
import malilib.util.game.wrap.EntityWrap;
import minihud.config.Configs;
import minihud.config.RendererToggle;

public class OverlayRendererSpawnableChunks extends MiniHudOverlayRenderer
{
    protected final RendererToggle toggle;
    protected BlockPos posCenter = BlockPos.ORIGIN;
    @Nullable protected BlockPos newPos;
    protected double overlayTopY;
    protected double topY = 256;

    public OverlayRendererSpawnableChunks(RendererToggle toggle)
    {
        this.toggle = toggle;
    }

    @Override
    public void onEnabled()
    {
        super.onEnabled();

        Vec3d pos = EntityWrap.getCameraEntityPosition();
        this.overlayTopY = pos.y;
    }

    @Override
    public boolean shouldRender()
    {
        return this.toggle.isRendererEnabled();
    }

    @Override
    public boolean needsUpdate(Entity entity)
    {
        if (this.toggle == RendererToggle.SPAWNABLE_CHUNKS_FIXED)
        {
            return this.newPos != null;
        }
        else
        {
            int ex = EntityWrap.getChunkX(entity);
            int ez = EntityWrap.getChunkZ(entity);
            int lx = this.lastUpdatePos.getX();
            int lz = this.lastUpdatePos.getZ();

            return ex != lx || ez != lz;
        }
    }

    @Override
    public void update(Vec3d cameraPos, Entity entity)
    {
        if (this.toggle == RendererToggle.SPAWNABLE_CHUNKS_FIXED)
        {
            if (this.newPos != null)
            {
                this.posCenter = this.newPos;
                this.newPos = null;
            }
            else
            {
                this.posCenter = new BlockPos(this.lastUpdatePos.getX() << 4, 0, this.lastUpdatePos.getZ() << 4);
            }
        }
        else
        {
            this.posCenter = EntityWrap.getEntityBlockPos(entity);
        }

        int centerX = this.posCenter.getX() >> 4;
        int centerZ = this.posCenter.getZ() >> 4;
        final Color4f color = this.toggle == RendererToggle.SPAWNABLE_CHUNKS_FIXED ?
                Configs.Colors.SPAWNABLE_CHUNKS_FIXED.getColor() :
                Configs.Colors.SPAWNABLE_CHUNKS_PLAYER.getColor();

        this.lastUpdatePos = new BlockPos(centerX, 0, centerZ);

        int r = 7;
        BlockPos pos1 = new BlockPos( (centerX - r    ) << 4,              0,  (centerZ - r    ) << 4     );
        BlockPos pos2 = new BlockPos(((centerX + r + 1) << 4) - 1, this.topY, ((centerZ + r + 1) << 4) - 1);

        this.startBuffers();

        RenderUtils.renderWallsWithLines(pos1, pos2, cameraPos, 16, 16, true, color, this.quadBuilder, this.lineBuilder);

        this.uploadBuffers();
    }

    @Override
    public String getSaveId()
    {
        if (this.toggle == RendererToggle.SPAWNABLE_CHUNKS_FIXED)
        {
            return "spawnable_chunks_fixed";
        }
        else
        {
            return "spawnable_chunks_player";
        }
    }

    @Nullable
    @Override
    public JsonObject toJson()
    {
        JsonObject obj = super.toJson();
        obj.add("pos", JsonUtils.blockPosToJson(this.posCenter));
        obj.add("top_y", new JsonPrimitive(this.topY));
        return obj;
    }

    @Override
    public void fromJson(JsonObject obj)
    {
        super.fromJson(obj);

        this.topY = JsonUtils.getDoubleOrDefault(obj, "top_y", 80);

        BlockPos pos = JsonUtils.getBlockPos(obj, "pos");

        if (pos != null && this.toggle == RendererToggle.SPAWNABLE_CHUNKS_FIXED)
        {
            this.newPos = pos;
        }
    }
}
