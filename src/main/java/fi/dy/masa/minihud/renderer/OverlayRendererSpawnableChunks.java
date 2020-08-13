package fi.dy.masa.minihud.renderer;

import javax.annotation.Nullable;
import org.lwjgl.opengl.GL11;
import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.vertex.DefaultVertexFormats;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Vec3d;
import fi.dy.masa.malilib.render.overlay.BaseRenderObject;
import fi.dy.masa.malilib.util.data.Color4f;
import fi.dy.masa.malilib.util.JsonUtils;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.RendererToggle;

public class OverlayRendererSpawnableChunks extends OverlayRendererBase
{
    @Nullable public static BlockPos newPos;
    public static double overlayTopY;

    protected final RendererToggle toggle;
    protected BlockPos posCenter = BlockPos.ORIGIN;
    protected double topY = 256;

    public OverlayRendererSpawnableChunks(RendererToggle toggle)
    {
        this.toggle = toggle;
    }

    @Override
    public boolean shouldRender(Minecraft mc)
    {
        return this.toggle.isRendererEnabled();
    }

    @Override
    public boolean needsUpdate(Entity entity, Minecraft mc)
    {
        /*
        if (this.topY != overlayTopY)
        {
            return true;
        }
        */

        if (this.toggle == RendererToggle.OVERLAY_SPAWNABLE_CHUNKS_FIXED)
        {
            return newPos != null;
        }
        else
        {
            int ex = ((int) Math.floor(entity.posX)) >> 4;
            int ez = ((int) Math.floor(entity.posZ)) >> 4;
            int lx = this.lastUpdatePos.getX();
            int lz = this.lastUpdatePos.getZ();

            return ex != lx || ez != lz;
        }
    }

    @Override
    public void update(Vec3d cameraPos, Entity entity, Minecraft mc)
    {
        if (this.toggle == RendererToggle.OVERLAY_SPAWNABLE_CHUNKS_FIXED)
        {
            if (newPos != null)
            {
                this.posCenter = newPos;
                newPos = null;
            }
            else
            {
                this.posCenter = new BlockPos(this.lastUpdatePos.getX() << 4, 0, this.lastUpdatePos.getZ() << 4);
            }
        }
        else
        {
            this.posCenter = new BlockPos(entity);
        }

        int centerX = this.posCenter.getX() >> 4;
        int centerZ = this.posCenter.getZ() >> 4;
        int r = 7;
        final Color4f color = this.toggle == RendererToggle.OVERLAY_SPAWNABLE_CHUNKS_FIXED ?
                Configs.Colors.SPAWNABLE_CHUNKS_FIXED.getColor() :
                Configs.Colors.SPAWNABLE_CHUNKS_PLAYER.getColor();

        this.lastUpdatePos = new BlockPos(centerX, 0, centerZ);
        //this.topY = overlayTopY;

        BaseRenderObject renderQuads = this.renderObjects.get(0);
        BaseRenderObject renderLines = this.renderObjects.get(1);
        BUFFER_1.begin(renderQuads.getGlMode(), DefaultVertexFormats.POSITION_COLOR);
        BUFFER_2.begin(renderLines.getGlMode(), DefaultVertexFormats.POSITION_COLOR);

        BlockPos pos1 = new BlockPos( (centerX - r    ) << 4,              0,  (centerZ - r    ) << 4     );
        BlockPos pos2 = new BlockPos(((centerX + r + 1) << 4) - 1, this.topY, ((centerZ + r + 1) << 4) - 1);

        RenderUtils.renderWallsWithLines(pos1, pos2, cameraPos, 16, 16, true, color, BUFFER_1, BUFFER_2);

        BUFFER_1.finishDrawing();
        BUFFER_2.finishDrawing();

        renderQuads.uploadData(BUFFER_1);
        renderLines.uploadData(BUFFER_2);
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
        if (this.toggle == RendererToggle.OVERLAY_SPAWNABLE_CHUNKS_FIXED)
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
        JsonObject obj = new JsonObject();
        obj.add("pos", JsonUtils.blockPosToJson(this.posCenter));
        obj.add("y_top", new JsonPrimitive(this.topY));
        return obj;
    }

    @Override
    public void fromJson(JsonObject obj)
    {
        this.topY = JsonUtils.getDouble(obj, "y_top");

        BlockPos pos = JsonUtils.blockPosFromJson(obj, "pos");

        if (pos != null && this.toggle == RendererToggle.OVERLAY_SPAWNABLE_CHUNKS_FIXED)
        {
            newPos = pos;
        }
    }
}
