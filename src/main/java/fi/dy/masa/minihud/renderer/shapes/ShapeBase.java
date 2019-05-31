package fi.dy.masa.minihud.renderer.shapes;

import java.util.List;
import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;
import fi.dy.masa.malilib.interfaces.IRangeChangeListener;
import fi.dy.masa.malilib.util.Color4f;
import fi.dy.masa.malilib.util.JsonUtils;
import fi.dy.masa.malilib.util.LayerRange;
import fi.dy.masa.malilib.util.StringUtils;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.renderer.OverlayRendererBase;
import fi.dy.masa.minihud.renderer.shapes.ShapeManager.ShapeTypes;
import net.minecraft.client.Minecraft;
import net.minecraft.entity.Entity;

public abstract class ShapeBase extends OverlayRendererBase implements IRangeChangeListener
{
    protected final ShapeTypes type;
    protected final LayerRange layerRange;
    protected final Minecraft mc;
    protected Color4f color;
    protected boolean enabled;
    protected boolean needsUpdate;

    public ShapeBase(ShapeTypes type, Color4f color)
    {
        this.type = type;
        this.color = color;
        this.layerRange = new LayerRange(this);
        this.mc = Minecraft.getInstance();
        this.needsUpdate = true;
    }

    public abstract List<String> getWidgetHoverLines();

    public ShapeTypes getType()
    {
        return this.type;
    }

    public LayerRange getLayerRange()
    {
        return this.layerRange;
    }

    public Color4f getColor()
    {
        return this.color;
    }

    public void setColorFromString(String newValue)
    {
        int newColor = StringUtils.getColor(newValue, 0);

        if (newColor != this.color.intValue)
        {
            this.color = Color4f.fromColor(newColor);
            this.setNeedsUpdate();
        }
    }

    public boolean isEnabled()
    {
        return enabled;
    }

    public void toggleEnabled()
    {
        this.enabled = ! this.enabled;

        if (this.enabled)
        {
            this.setNeedsUpdate();
        }
    }

    public void setNeedsUpdate()
    {
        this.needsUpdate = true;
    }

    @Override
    public boolean shouldRender(Minecraft mc)
    {
        return this.enabled && RendererToggle.SHAPE_RENDERER.getBooleanValue();
    }

    @Override
    public boolean needsUpdate(Entity entity, Minecraft mc)
    {
        return this.needsUpdate;
    }

    @Override
    public void updateAll()
    {
        this.setNeedsUpdate();
    }

    @Override
    public void updateBetweenX(int minX, int maxX)
    {
        this.setNeedsUpdate();
    }

    @Override
    public void updateBetweenY(int minY, int maxY)
    {
        this.setNeedsUpdate();
    }

    @Override
    public void updateBetweenZ(int minZ, int maxZ)
    {
        this.setNeedsUpdate();
    }

    public JsonObject toJson()
    {
        JsonObject obj = new JsonObject();

        obj.add("type", new JsonPrimitive(this.type.getId()));
        obj.add("enabled", new JsonPrimitive(this.enabled));
        obj.add("layers", this.layerRange.toJson());

        return obj;
    }

    public void fromJson(JsonObject obj)
    {
        this.enabled = JsonUtils.getBoolean(obj, "enabled");

        if (JsonUtils.hasObject(obj, "layers"))
        {
            this.layerRange.fromJson(JsonUtils.getNestedObject(obj, "layers", false));
        }
    }
}
