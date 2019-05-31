package fi.dy.masa.minihud.renderer;

import java.util.ArrayList;
import java.util.List;
import javax.annotation.Nullable;
import com.google.gson.JsonObject;
import net.minecraft.client.renderer.BufferBuilder;
import net.minecraft.client.renderer.GlStateManager;
import net.minecraft.client.renderer.OpenGlHelper;
import net.minecraft.util.math.BlockPos;

public abstract class OverlayRendererBase implements IOverlayRenderer
{
    protected static final BufferBuilder BUFFER_1 = new BufferBuilder(2097152);
    protected static final BufferBuilder BUFFER_2 = new BufferBuilder(2097152);
    protected static final BufferBuilder BUFFER_3 = new BufferBuilder(2097152);

    protected final List<RenderObjectBase> renderObjects = new ArrayList<>();
    protected float glLineWidth = 1f;
    protected BlockPos lastUpdatePos = BlockPos.ORIGIN;
    private BlockPos position = BlockPos.ORIGIN;

    protected void preRender(double x, double y, double z)
    {
        GlStateManager.lineWidth(this.glLineWidth);
        GlStateManager.translated(this.position.getX() - x, this.position.getY() - y, this.position.getZ() - z);
    }

    @Override
    public void draw(double x, double y, double z)
    {
        GlStateManager.pushMatrix();
        this.preRender(x, y, z);

        for (RenderObjectBase obj : this.renderObjects)
        {
            obj.draw();
        }

        GlStateManager.popMatrix();
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

    protected void setPosition(BlockPos pos)
    {
        this.position = pos;

        BUFFER_1.setTranslation(-pos.getX(), -pos.getY(), -pos.getZ());
        BUFFER_2.setTranslation(-pos.getX(), -pos.getY(), -pos.getZ());
        BUFFER_3.setTranslation(-pos.getX(), -pos.getY(), -pos.getZ());
    }

    /**
     * Allocates a new VBO or display list, adds it to the list, and returns it
     * @param glMode
     * @return
     */
    protected RenderObjectBase allocateBuffer(int glMode)
    {
        RenderObjectBase obj;

        if (OpenGlHelper.useVbo())
        {
            obj = new RenderObjectVbo(glMode);
        }
        else
        {
            obj = new RenderObjectDisplayList(glMode);
        }

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
