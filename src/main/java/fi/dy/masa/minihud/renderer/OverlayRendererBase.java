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

public abstract class OverlayRendererBase implements IOverlayRenderer
{
    protected static final BufferBuilder BUFFER_1 = new BufferBuilder(2097152);
    protected static final BufferBuilder BUFFER_2 = new BufferBuilder(2097152);
    //protected static final BufferBuilder BUFFER_3 = new BufferBuilder(2097152);

    protected final List<RenderObjectBase> renderObjects = new ArrayList<>();
    protected float glLineWidth = 1f;
    protected BlockPos lastUpdatePos = BlockPos.ORIGIN;

    protected void preRender(double x, double y, double z)
    {
        RenderSystem.lineWidth(this.glLineWidth);
    }

    protected void postRender(double x, double y, double z)
    {
    }

    @Override
    public void draw(double x, double y, double z, MatrixStack matrixStack)
    {
        RenderSystem.pushMatrix();
        this.preRender(x, y, z);

        matrixStack.push();
        matrixStack.translate(-x, -y, -z);

        for (RenderObjectBase obj : this.renderObjects)
        {
            obj.draw(matrixStack);
        }

        matrixStack.pop();
        this.postRender(x, y, z);

        RenderSystem.popMatrix();
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
    protected RenderObjectBase allocateBuffer(int glMode)
    {
        return this.allocateBuffer(glMode, VertexFormats.POSITION_COLOR);
    }

    /**
     * Allocates a new VBO or display list, adds it to the list, and returns it
     * @param glMode
     * @return
     */
    protected RenderObjectBase allocateBuffer(int glMode, VertexFormat format)
    {
        RenderObjectBase obj = new RenderObjectVbo(glMode, format);
        this.renderObjects.add(obj);
        return obj;
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
