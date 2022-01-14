package fi.dy.masa.minihud.renderer.shapes;

import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.List;
import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;
import net.minecraft.client.MinecraftClient;
import net.minecraft.entity.Entity;
import fi.dy.masa.malilib.interfaces.IRangeChangeListener;
import fi.dy.masa.malilib.util.Color4f;
import fi.dy.masa.malilib.util.JsonUtils;
import fi.dy.masa.malilib.util.LayerRange;
import fi.dy.masa.malilib.util.StringUtils;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.renderer.OverlayRendererBase;
import fi.dy.masa.minihud.util.ShapeRenderType;

public abstract class ShapeBase extends OverlayRendererBase implements IRangeChangeListener
{
    protected static final DecimalFormat DEC_FMT = new DecimalFormat("#.##");

    protected final MinecraftClient mc;
    protected final ShapeType type;
    protected final LayerRange layerRange;
    protected String displayName;
    protected ShapeRenderType renderType;
    protected Color4f color;
    protected boolean enabled;
    protected boolean needsUpdate;

    public ShapeBase(ShapeType type, Color4f color)
    {
        this.mc = MinecraftClient.getInstance();
        this.type = type;
        this.color = color;
        this.layerRange = new LayerRange(this);
        this.renderType = ShapeRenderType.OUTER_EDGE;
        this.displayName = type.getDisplayName();
        this.needsUpdate = true;
    }

    public ShapeType getType()
    {
        return this.type;
    }

    public String getDisplayName()
    {
        return this.displayName;
    }

    public LayerRange getLayerRange()
    {
        return this.layerRange;
    }

    public Color4f getColor()
    {
        return this.color;
    }

    public ShapeRenderType getRenderType()
    {
        return this.renderType;
    }

    public void setDisplayName(String displayName)
    {
        this.displayName = displayName;
    }

    public void setRenderType(ShapeRenderType renderType)
    {
        this.renderType = renderType;
        this.setNeedsUpdate();
    }

    public void setColor(int newColor)
    {
        if (newColor != this.color.intValue)
        {
            this.color = Color4f.fromColor(newColor);
            this.setNeedsUpdate();
        }
    }

    public void setColorFromString(String newValue)
    {
        this.setColor(StringUtils.getColor(newValue, 0));
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
    public boolean shouldRender(MinecraftClient mc)
    {
        return this.enabled && RendererToggle.SHAPE_RENDERER.getBooleanValue();
    }

    @Override
    public boolean needsUpdate(Entity entity, MinecraftClient mc)
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

    public List<String> getWidgetHoverLines()
    {
        List<String> lines = new ArrayList<>();

        lines.add(StringUtils.translate("minihud.gui.hover.shape.type_value", this.type.getDisplayName()));

        return lines;
    }

    @Override
    public JsonObject toJson()
    {
        JsonObject obj = new JsonObject();

        obj.add("type", new JsonPrimitive(this.type.getId()));
        obj.add("color", new JsonPrimitive(this.color.intValue));
        obj.add("enabled", new JsonPrimitive(this.enabled));
        obj.add("display_name", new JsonPrimitive(this.displayName));
        obj.add("render_type", new JsonPrimitive(this.renderType.getStringValue()));
        obj.add("layers", this.layerRange.toJson());

        return obj;
    }

    @Override
    public void fromJson(JsonObject obj)
    {
        this.enabled = JsonUtils.getBoolean(obj, "enabled");

        if (JsonUtils.hasInteger(obj, "color"))
        {
            this.color = Color4f.fromColor(JsonUtils.getInteger(obj, "color"));
        }

        if (JsonUtils.hasObject(obj, "layers"))
        {
            this.layerRange.fromJson(JsonUtils.getNestedObject(obj, "layers", false));
        }

        if (JsonUtils.hasString(obj, "render_type"))
        {
            ShapeRenderType type = ShapeRenderType.fromStringStatic(obj.get("render_type").getAsString());

            if (type != null)
            {
                this.renderType = type;
            }
        }

        if (JsonUtils.hasString(obj, "display_name"))
        {
            this.displayName = obj.get("display_name").getAsString();
        }
    }

    public static String d2(double val)
    {
        return DEC_FMT.format(val);
    }
}
