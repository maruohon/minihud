package fi.dy.masa.minihud.renderer.shapes;

import java.util.ArrayList;
import java.util.List;
import javax.annotation.Nullable;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;
import fi.dy.masa.malilib.render.overlay.OverlayRendererContainer;
import fi.dy.masa.malilib.util.JsonUtils;

public class ShapeManager
{
    public static final ShapeManager INSTANCE = new ShapeManager();

    private final List<ShapeBase> shapes = new ArrayList<>();
    @Nullable private ShapeBase selectedShape;

    @Nullable
    public ShapeBase getSelectedShape()
    {
        return this.selectedShape;
    }

    public void setSelectedShape(@Nullable ShapeBase shape)
    {
        this.selectedShape = shape;
    }

    public List<ShapeBase> getAllShapes()
    {
        return this.shapes;
    }

    public void addShape(ShapeBase shape)
    {
        this.shapes.add(shape);

        OverlayRendererContainer.INSTANCE.addRenderer(shape);
    }

    public void removeShape(ShapeBase shape)
    {
        this.shapes.remove(shape);

        OverlayRendererContainer.INSTANCE.removeRenderer(shape);
    }

    public void clear()
    {
        for (ShapeBase shape : this.shapes)
        {
            OverlayRendererContainer.INSTANCE.removeRenderer(shape);
        }

        this.shapes.clear();
        this.selectedShape = null;
    }

    public JsonObject toJson()
    {
        JsonObject obj = new JsonObject();
        JsonArray arr = new JsonArray();
        int selected = -1;

        for (int i = 0; i < this.shapes.size(); ++i)
        {
            ShapeBase shape = this.shapes.get(i);
            arr.add(shape.toJson());

            if (this.selectedShape == shape)
            {
                selected = i;
            }
        }

        if (arr.size() > 0)
        {
            obj.add("shapes", arr);
        }

        if (selected != -1)
        {
            obj.add("selected", new JsonPrimitive(selected));
        }

        return obj;
    }

    public void fromJson(JsonObject obj)
    {
        this.clear();

        JsonUtils.readArrayElementsIfExists(obj, "shapes", this::readAndAddShape);
        int selected = JsonUtils.getIntegerOrDefault(obj, "selected", -1);

        if (selected >= 0 && selected < this.shapes.size())
        {
            this.selectedShape = this.shapes.get(selected);
        }
    }

    protected void readAndAddShape(JsonElement el)
    {
        if (el.isJsonObject() == false)
        {
            return;
        }

        JsonObject o = el.getAsJsonObject();
        String typeName = JsonUtils.getStringOrDefault(o, "type", "");
        ShapeType type = ShapeType.fromString(typeName);

        if (type != null)
        {
            ShapeBase shape = type.createShape();
            shape.fromJson(o);
            this.addShape(shape);
        }
    }
}
