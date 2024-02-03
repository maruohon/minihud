package minihud.renderer;

import java.util.Collection;
import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.ImmutableList;

import net.minecraft.entity.Entity;

import malilib.render.ShapeRenderUtils;
import malilib.util.data.Color4f;
import malilib.util.game.wrap.EntityWrap;
import malilib.util.game.wrap.GameUtils;
import malilib.util.position.BlockPos;
import malilib.util.position.IntBoundingBox;
import malilib.util.position.Vec3d;
import minihud.config.RendererToggle;
import minihud.data.structure.StructureData;
import minihud.data.structure.StructureStorage;
import minihud.data.structure.StructureType;
import minihud.util.MiscUtils;

public class OverlayRendererStructures extends MiniHudOverlayRenderer
{
    @Override
    public boolean shouldRender()
    {
        // TODO use a cached value for any types enabled?
        return RendererToggle.STRUCTURE_BOUNDING_BOXES.isRendererEnabled();
    }

    @Override
    public boolean needsUpdate(Entity entity)
    {
        int hysteresis = 16;

        return StructureStorage.INSTANCE.hasStructureDataChanged() ||
               Math.abs(EntityWrap.getX(entity) - this.lastUpdatePos.getX()) > hysteresis ||
               Math.abs(EntityWrap.getY(entity) - this.lastUpdatePos.getY()) > hysteresis ||
               Math.abs(EntityWrap.getZ(entity) - this.lastUpdatePos.getZ()) > hysteresis;
    }

    @Override
    public void update(Vec3d cameraPos, Entity entity)
    {
        this.startBuffers();
        this.updateStructures(this.lastUpdatePos, cameraPos);
        this.uploadBuffers();
    }

    protected void updateStructures(BlockPos playerPos, Vec3d cameraPos)
    {
        ArrayListMultimap<StructureType, StructureData> structures = StructureStorage.INSTANCE.getStructureDataAndClearDirtyFlag();
        int maxRange = (GameUtils.getRenderDistanceChunks() + 2) * 16;

        for (StructureType type : structures.keySet())
        {
            if (type.isEnabled())
            {
                Collection<StructureData> structureData = structures.get(type);

                if (structureData.isEmpty() == false)
                {
                    this.renderStructuresWithinRange(type, structureData, playerPos, maxRange, cameraPos);
                }
            }
        }
    }

    protected void renderStructuresWithinRange(StructureType type, Collection<StructureData> structureData,
                                               BlockPos playerPos, int maxRange, Vec3d cameraPos)
    {
        for (StructureData structure : structureData)
        {
            if (MiscUtils.isStructureWithinRange(structure.getEnclosingBox(), playerPos, maxRange))
            {
                this.renderStructure(type, structure, cameraPos);
            }
        }
    }

    protected void renderStructure(StructureType type, StructureData structure, Vec3d cameraPos)
    {
        Color4f color = type.getToggle().getColorMain().getColor();
        ImmutableList<IntBoundingBox> components = structure.getComponents();

        ShapeRenderUtils.renderBoxSidesAndEdges(structure.getEnclosingBox(), color, cameraPos, this.quadBuilder, this.lineBuilder);
        int size = components.size();

        // Don't render the component box if there is only one component
        // and the box is the same as the enclosing box (for example a proper, "non-broken" Swamp Hut)
        if (size >= 1 &&
            (size > 1 || components.get(0).equals(structure.getEnclosingBox()) == false))
        {
            color = type.getToggle().getColorComponents().getColor();

            for (IntBoundingBox bb : components)
            {
                ShapeRenderUtils.renderBoxSidesAndEdges(bb, color, cameraPos, this.quadBuilder, this.lineBuilder);
            }
        }
    }
}
