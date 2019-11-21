package fi.dy.masa.minihud.renderer;

import java.util.ArrayList;
import java.util.List;
import javax.annotation.Nullable;
import com.google.gson.JsonObject;
import com.mojang.blaze3d.platform.GlStateManager;
import net.minecraft.client.render.BufferBuilder;
import net.minecraft.client.util.math.MatrixStack;
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
    private MatrixStack matrixStack;

    protected void preRender(double x, double y, double z)
    {
        GlStateManager.lineWidth(this.glLineWidth);
        //RenderSystem.translated(this.position.getX() - x, this.position.getY() - y, this.position.getZ() - z);
    }

    protected void setMatrixQueue(MatrixStack matrixQueue)
    {
        this.matrixStack = matrixQueue;
    }

    protected MatrixStack getMatrixQueue()
    {
        return this.matrixStack;
    }

    @Override
    public void draw(double x, double y, double z, MatrixStack matrixStack)
    {
        GlStateManager.pushMatrix();
        this.preRender(x, y, z);

        matrixStack.push();
        matrixStack.translate(-x, -y, -z);
        //this.matrixQueue.method_22904(this.position.getX() - x, this.position.getY() - y, this.position.getZ() - z);

        for (RenderObjectBase obj : this.renderObjects)
        {
            obj.draw(matrixStack);
        }

        matrixStack.pop();

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
    }

    protected BufferBuilder vertex(BufferBuilder buffer, double x, double y, double z)
    {
        buffer.vertex(x - this.position.getX(), y - this.position.getY(), z - this.position.getZ());
        return buffer;
    }

    /**
     * Allocates a new VBO or display list, adds it to the list, and returns it
     * @param glMode
     * @return
     */
    protected RenderObjectBase allocateBuffer(int glMode)
    {
        RenderObjectBase obj = new RenderObjectVbo(glMode);
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
