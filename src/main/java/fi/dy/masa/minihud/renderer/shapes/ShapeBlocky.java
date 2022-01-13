package fi.dy.masa.minihud.renderer.shapes;

import java.util.function.Consumer;
import org.lwjgl.opengl.GL11;
import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;
import com.mojang.blaze3d.systems.RenderSystem;
import net.minecraft.client.MinecraftClient;
import net.minecraft.client.render.VertexFormat;
import net.minecraft.client.util.math.MatrixStack;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Box;
import net.minecraft.util.math.Matrix4f;
import net.minecraft.util.math.Vec3d;
import fi.dy.masa.malilib.util.Color4f;
import fi.dy.masa.malilib.util.EntityUtils;
import fi.dy.masa.malilib.util.IntBoundingBox;
import fi.dy.masa.malilib.util.JsonUtils;
import it.unimi.dsi.fastutil.longs.LongOpenHashSet;

public abstract class ShapeBlocky extends ShapeBase
{
    protected Box renderPerimeter = ShapeBox.EMPTY_BOX;
    private boolean combineQuads;

    public ShapeBlocky(ShapeType type, Color4f color)
    {
        super(type, color);
    }

    public boolean getCombineQuads()
    {
        return this.combineQuads;
    }

    public boolean toggleCombineQuads()
    {
        this.combineQuads = ! this.combineQuads;
        this.setNeedsUpdate();
        return this.combineQuads;
    }

    protected void setRenderPerimeter(Vec3d center, double range)
    {
        this.renderPerimeter = new Box(center.x - range, center.y - range, center.z - range,
                                       center.x + range, center.y + range, center.z + range);
    }

    @Override
    public boolean shouldRender(MinecraftClient mc)
    {
        Entity entity = EntityUtils.getCameraEntity();
        return super.shouldRender(mc) && entity != null && this.renderPerimeter.contains(entity.getPos());
    }

    @Override
    public void allocateGlResources()
    {
        this.allocateBuffer(VertexFormat.DrawMode.QUADS);
    }

    @Override
    public void draw(MatrixStack matrixStack, Matrix4f projMatrix)
    {
        this.preRender();

        this.renderObjects.get(0).draw(matrixStack, projMatrix);

        // Render the lines as quads with glPolygonMode(GL_LINE)
        RenderSystem.polygonMode(GL11.GL_FRONT_AND_BACK, GL11.GL_LINE);
        RenderSystem.disableBlend();
        this.renderObjects.get(0).draw(matrixStack, projMatrix);
        RenderSystem.polygonMode(GL11.GL_FRONT_AND_BACK, GL11.GL_FILL);
        RenderSystem.enableBlend();
    }

    @Override
    public JsonObject toJson()
    {
        JsonObject obj = super.toJson();
        obj.add("combine_quads", new JsonPrimitive(this.combineQuads));
        return obj;
    }

    @Override
    public void fromJson(JsonObject obj)
    {
        super.fromJson(obj);
        this.combineQuads = JsonUtils.getBooleanOrDefault(obj, "combine_quads", false);
    }

    protected Consumer<BlockPos.Mutable> getPositionCollector(LongOpenHashSet positionsOut)
    {
        IntBoundingBox box = this.layerRange.getExpandedBox(this.mc.world, 0);

        Consumer<BlockPos.Mutable> positionCollector = (pos) -> {
            if (box.containsPos(pos))
            {
                positionsOut.add(pos.asLong());
            }
        };

        return positionCollector;
    }
}
