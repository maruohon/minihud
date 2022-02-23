package fi.dy.masa.minihud.renderer.shapes;

import java.util.ArrayList;
import java.util.List;
import javax.annotation.Nullable;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;
import fi.dy.masa.malilib.util.JsonUtils;
import fi.dy.masa.minihud.renderer.RenderContainer;

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

        RenderContainer.INSTANCE.addRenderer(shape);
    }

    public void removeShape(ShapeBase shape)
    {
        this.shapes.remove(shape);

        RenderContainer.INSTANCE.removeRenderer(shape);
    }

    public void clear()
    {
        for (ShapeBase shape : this.shapes)
        {
            RenderContainer.INSTANCE.removeRenderer(shape);
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

        if (JsonUtils.hasArray(obj, "shapes"))
        {
            JsonArray arr = obj.get("shapes").getAsJsonArray();

            for (int i = 0; i < arr.size(); ++i)
            {
                JsonElement el = arr.get(i);

                if (el.isJsonObject())
                {
                    JsonObject o = el.getAsJsonObject();

                    if (JsonUtils.hasString(o, "type"))
                    {
                        ShapeType type = ShapeType.fromString(JsonUtils.getString(o, "type"));

                        if (type != null)
                        {
                            ShapeBase shape = type.createShape();
                            shape.fromJson(o);
                            this.addShape(shape);
                        }
                    }
                }
            }

            if (JsonUtils.hasInteger(obj, "selected"))
            {
                int selected = JsonUtils.getInteger(obj, "selected");

                if (selected >= 0 && selected < this.shapes.size())
                {
                    this.selectedShape = this.shapes.get(selected);
                }
            }
        }
    }
}
