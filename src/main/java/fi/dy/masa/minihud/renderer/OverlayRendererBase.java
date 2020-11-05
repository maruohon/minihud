package fi.dy.masa.minihud.renderer;

import java.util.ArrayList;
import java.util.List;
import javax.annotation.Nullable;
import com.google.gson.JsonObject;
import com.mojang.blaze3d.systems.RenderSystem;
import net.minecraft.client.render.BufferBuilder;
import net.minecraft.client.render.VertexFormat;
import net.minecraft.client.render.VertexFormats;
import net.minecraft.client.util.math.MatrixStack;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Vec3d;

public abstract class OverlayRendererBase implements IOverlayRenderer
{
    protected static final BufferBuilder BUFFER_1 = new BufferBuilder(2097152);
    protected static final BufferBuilder BUFFER_2 = new BufferBuilder(2097152);
    //protected static final BufferBuilder BUFFER_3 = new BufferBuilder(2097152);

    protected final List<RenderObjectBase> renderObjects = new ArrayList<>();
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
        RenderSystem.lineWidth(this.glLineWidth);

        if (this.renderThrough)
        {
            RenderSystem.disableDepthTest();
            //RenderSystem.depthMask(false);
        }
    }

    protected void postRender()
    {
        if (this.renderThrough)
        {
            RenderSystem.enableDepthTest();
            //RenderSystem.depthMask(true);
        }
    }

    @Override
    public void draw(MatrixStack matrixStack)
    {
        this.preRender();

        for (RenderObjectBase obj : this.renderObjects)
        {
            obj.draw(matrixStack);
        }

        this.postRender();
    }

    @Override
    public void deleteGlResources()
    {
        for (RenderObjectBase obj : this.renderObjects)
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
    protected RenderObjectBase allocateBuffer(VertexFormat.class_5596 glMode)
    {
        return this.allocateBuffer(glMode, VertexFormats.POSITION_COLOR);
    }

    /**
     * Allocates a new VBO or display list, adds it to the list, and returns it
     * @param glMode
     * @return
     */
    protected RenderObjectBase allocateBuffer(VertexFormat.class_5596 glMode, VertexFormat format)
    {
        RenderObjectBase obj = new RenderObjectVbo(glMode, format);
        this.renderObjects.add(obj);
        return obj;
    }

    @Override
    public void allocateGlResources()
    {
        this.allocateBuffer(VertexFormat.class_5596.field_27382); // QUADS
        this.allocateBuffer(VertexFormat.class_5596.field_27377); // LINES
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
