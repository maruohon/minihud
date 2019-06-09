package fi.dy.masa.minihud.renderer.shapes;

import java.util.ArrayList;
import java.util.List;
import javax.annotation.Nullable;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;
import fi.dy.masa.malilib.util.JsonUtils;
import fi.dy.masa.malilib.util.StringUtils;
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

        RenderContainer.INSTANCE.addShapeRenderer(shape);
    }

    public void removeShape(ShapeBase shape)
    {
        this.shapes.remove(shape);

        RenderContainer.INSTANCE.removeShapeRenderer(shape);
    }

    public void clear()
    {
        for (ShapeBase shape : this.shapes)
        {
            RenderContainer.INSTANCE.removeShapeRenderer(shape);
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
                        ShapeTypes type = ShapeTypes.fromString(JsonUtils.getString(o, "type"));

                        if (type != null)
                        {
                            switch (type)
                            {
                                case DESPAWN_SPHERE:
                                    ShapeDespawnSphere shape = new ShapeDespawnSphere();
                                    shape.fromJson(o);
                                    this.addShape(shape);
                                    break;
                            }
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

    public enum ShapeTypes
    {
        DESPAWN_SPHERE      ("despawn_sphere",  "minihud.label.shapes.despawn_sphere");

        private final String id;
        private final String translationKey;

        private ShapeTypes(String id, String translationKey)
        {
            this.id = id;
            this.translationKey = translationKey;
        }

        public String getId()
        {
            return this.id;
        }

        public String getDisplayName()
        {
            return StringUtils.translate(this.translationKey);
        }

        @Nullable
        public static ShapeTypes fromString(String id)
        {
            for (ShapeTypes type : ShapeTypes.values())
            {
                if (type.getId().equalsIgnoreCase(id))
                {
                    return type;
                }
            }

            return null;
        }
    }
}
