package fi.dy.masa.minihud.renderer;

import java.util.ArrayList;
import java.util.List;
import javax.annotation.Nullable;
import com.google.gson.JsonObject;
import net.minecraft.client.renderer.BufferBuilder;
import net.minecraft.client.renderer.GlStateManager;
import net.minecraft.client.renderer.OpenGlHelper;
import net.minecraft.client.renderer.vertex.DefaultVertexFormats;
import net.minecraft.client.renderer.vertex.VertexFormat;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Vec3d;
import fi.dy.masa.malilib.render.overlay.BaseRenderObject;
import fi.dy.masa.malilib.render.overlay.DisplayListRenderObject;
import fi.dy.masa.malilib.render.overlay.VboRenderObject;

public abstract class OverlayRendererBase implements IOverlayRenderer
{
    protected static final BufferBuilder BUFFER_1 = new BufferBuilder(2097152);
    protected static final BufferBuilder BUFFER_2 = new BufferBuilder(2097152);
    protected static final BufferBuilder BUFFER_3 = new BufferBuilder(2097152);

    protected final List<BaseRenderObject> renderObjects = new ArrayList<>();
    protected boolean renderThrough = false;
    protected float glLineWidth = 1f;
    protected BlockPos lastUpdatePos = BlockPos.ORIGIN;
    private Vec3d updateCameraPos = Vec3d.ZERO;

    @Override
    public final Vec3d getUpdatePosition()
    {
        return this.updateCameraPos;
    }

    @Override
    public final void setUpdatePosition(Vec3d cameraPosition)
    {
        this.updateCameraPos = cameraPosition;
    }

    protected void preRender()
    {
        GlStateManager.glLineWidth(this.glLineWidth);

        if (this.renderThrough)
        {
            GlStateManager.disableDepth();
            //GlStateManager.depthMask(false);
        }
    }

    protected void postRender()
    {
        if (this.renderThrough)
        {
            GlStateManager.enableDepth();
            //GlStateManager.depthMask(true);
        }
    }

    @Override
    public void draw()
    {
        this.preRender();

        for (BaseRenderObject obj : this.renderObjects)
        {
            obj.draw();
        }

        this.postRender();
    }

    @Override
    public void deleteGlResources()
    {
        for (BaseRenderObject obj : this.renderObjects)
        {
            obj.deleteGlResources();
        }

        this.renderObjects.clear();
    }

    /**
     * Allocates a new VBO or display list, adds it to the list, and returns it
     * @param glMode
     * @return
     */
    protected BaseRenderObject allocateBuffer(int glMode)
    {
        return this.allocateBuffer(glMode, DefaultVertexFormats.POSITION_COLOR, VboRenderObject::setupArrayPointersPosColor);
    }

    /**
     * Allocates a new VBO or display list, adds it to the list, and returns it
     * @param glMode
     * @param vertexFormat
     * @param func the function to set up the array pointers according to the used vertex format
     * @return
     */
    protected BaseRenderObject allocateBuffer(int glMode, VertexFormat vertexFormat, VboRenderObject.IArrayPointerSetter func)
    {
        BaseRenderObject obj;

        if (OpenGlHelper.useVbo())
        {
            obj = new VboRenderObject(glMode, vertexFormat, func);
        }
        else
        {
            obj = new DisplayListRenderObject(glMode, vertexFormat);
        }

        this.renderObjects.add(obj);

        return obj;
    }

    public void setRenderThrough(boolean renderThrough)
    {
        this.renderThrough = renderThrough;
    }

    public String getSaveId()
    {
        return "";
    }

    @Nullable
    public JsonObject toJson()
    {
        return null;
    }

    public void fromJson(JsonObject obj)
    {
    }
}
