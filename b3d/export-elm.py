import bpy, math

def fl_str(l): # formatted list to string
    sl= str(l).split(', ')
    ol= []
    lenght= 24
    for s in sl[:-1]:
        lenght += len(s) + 2
        if lenght > 124:
            ol.append(s + '\n    , ')
            lenght= 6
        else:
            ol.append(s + ', ')
    ol.append(sl[-1])
    return ''.join(ol)

def export_elm(meshName, objName, pathName, useSecondUVMap=True, useSkelAnim=True):
    mesh = bpy.data.meshes[meshName]
    obj = bpy.data.objects[objName]
    u = open(pathName, 'w+')
    fName= meshName.lower()+'Mesh'
    u.write(fName+' : Mesh')
    u.write('\n'+fName+' =')
    u.write('\n    { noExtend= True') #float3[] extent = [(-80, -20, -90), (80, 25, 90)]''')
    u.write('\n    , doubleSided= False')
    u.write('\n    , faceVertexCounts= ' + fl_str([len(p.vertices) for p in mesh.polygons]))
    indices = []
    for p in mesh.polygons:
        for v in p.vertices:
            indices.append(v.numerator)
    u.write('\n    , faceVertexIndices= ' + fl_str(indices))
    u.write('\n    , points= '+fl_str([ (v.undeformed_co.x, v.undeformed_co.z, -v.undeformed_co.y) for v in mesh.vertices ]))
    # UV maping, in USD called ST
    uvs0 = [(0.0, 0.0)] * len(mesh.vertices)
    for face in mesh.polygons:
        for vert_idx, loop_idx in zip(face.vertices, face.loop_indices):
            uv = mesh.uv_layers[0].data[loop_idx].uv
            uvs0[vert_idx] = (uv.x, uv.y)
    u.write('\n    , st= ' + fl_str(uvs0))
    u.write('\n    , stIndices= Just ' + fl_str(indices))
    u.write('\n    , stInterpolation= FaceVarying')
    if useSecondUVMap:
        uvs1 = [(0.0, 0.0)] * len(mesh.vertices)
        for face in mesh.polygons:
            for vert_idx, loop_idx in zip(face.vertices, face.loop_indices):
                uv = mesh.uv_layers[1].data[loop_idx].uv
                uvs1[vert_idx] = (uv.x, uv.y)
        u.write('\n    , st1= ' + fl_str(uvs1))
        u.write('\n    , st1Indices= Just ' + fl_str(indices))
    else:
        u.write('\n    , st1= []')
        u.write('\n    , st1Indices= Nothing')
    u.write('\n    , st1Interpolation= FaceVarying')
    u.write('\n    , normals= ' + fl_str([ (v.normal.x, v.normal.z, -v.normal.y) for v in mesh.vertices ]))
    u.write('\n    , normalIndices= Just ' + fl_str(indices))
    u.write('\n    , normalsInterpolation= FaceVarying')
    u.write('\n    , subdivisionScheme= None')
    u.write('\n    }')
    if useSkelAnim:
        # prepare weight data
        maxGrps = 0
        for v in mesh.vertices:
            l = 0
            for g in v.groups:
                if g.weight > 0.0001:
                    l += 1
                if l > maxGrps:
                    maxGrps = l
        ids = '\n        , jointIndices = ['
        ws  = '\n        , jointWeights = ['
        for v in mesh.vertices:
            wSum = 0.0
            for g in v.groups:
                if g.weight > 0.0001:
                    wSum += g.weight
            if wSum < 0.0001:
                i = w = ' 0,'*maxGrps
            else:
                i = ''
                w = ''
                count = 0
                for g in v.groups:
                    if g.weight > 0.0001:
                        count += 1
                        i += ' '+str(g.group)+','
                        newW = g.weight/wSum
                        if newW < 0.0005:
                            newW = '0'
                        elif newW > 0.9995:
                            newW = '1'
                        w += ' '+str(newW)+','
                fill = ' 0,'*(maxGrps-count)
                i += fill
                w += fill
            ids += i 
            ws += w
        ids = ids[:-1]+' ]'
        ws  =  ws[:-1]+' ]'
        u.write('\n\n\n'+ meshName.lower() +'SkelAnim= Just')
        u.write('\n        { geomBindTransform= [ [1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 1, 0], [0, 0, 0, 1] ]')
        u.write('''\n        , bindTransforms=
        , [[1.0, 0, 0, 0], [0, 1, 0, 0], [0, 0, 1, 0], [0, 0, 0, 1]]
        , [[1.0, 0, 0, 0], [0, 1, 0, 0], [0, 0, 1, 0], [0, 0, 0, 1]]
        , [[1.0, 0, 0, 0], [0, 1, 0, 0], [0, 0, 1, 0], [0, 0, 0, 1]]
        , [[1.0, 0, 0, 0], [0, 1, 0, 0], [0, 0, 1, 0], [0, 0, 0, 1]]
        , [[1.0, 0, 0, 0], [0, 1, 0, 0], [0, 0, 1, 0], [0, 0, 0, 1]]
        , [[1.0, 0, 0, 0], [0, 1, 0, 0], [0, 0, 1, 0], [0, 0, 0, 1]]
        , [[1.0, 0, 0, 0], [0, 1, 0, 0], [0, 0, 1, 0], [0, 0, 0, 1]]
        ]
''')
        u.write('''\n        , restTransforms=
        , [[1.0, 0, 0, 0], [0, 1, 0, 0], [0, 0, 1, 0], [0, 0, 0, 1]]
        , [[1.0, 0, 0, 0], [0, 1, 0, 0], [0, 0, 1, 0], [0, 0, 0, 1]]
        , [[1.0, 0, 0, 0], [0, 1, 0, 0], [0, 0, 1, 0], [0, 0, 0, 1]]
        , [[1.0, 0, 0, 0], [0, 1, 0, 0], [0, 0, 1, 0], [0, 0, 0, 1]]
        , [[1.0, 0, 0, 0], [0, 1, 0, 0], [0, 0, 1, 0], [0, 0, 0, 1]]
        , [[1.0, 0, 0, 0], [0, 1, 0, 0], [0, 0, 1, 0], [0, 0, 0, 1]]
        , [[1.0, 0, 0, 0], [0, 1, 0, 0], [0, 0, 1, 0], [0, 0, 0, 1]]
        ]
''')
        u.write(ids)
        u.write(ws)
        u.write('\n        , elementSize= '+str(maxGrps))
        u.write('\n        , joints= ' + fl_str( [g.name for g in obj.vertex_groups] ))
        u.write('\n        , rotations= [ (0.0, [[0,0,0]]) ] --List (Float, List (List Float)) quatf[]')
        u.write('\n        , translations= [ (0.0, [(0,0,0)]) ] --List (Float, List (Float, Float, Float)) float3[]')
        u.write('\n        , scales= [ (0.0, [(0,0,0)]) ] --List (Float, List (Float, Float, Float)) half3[]')
        u.write('\n        }')
    else:
        u.write('\n\n\n'+ meshName.lower() +'SkelAnim= Nothing')
    u.close()

export_elm('RolleHinten', 'RolleHinten', '/Users/tkumlehn/Documents/ConfigBuilder/RolleHinten.elm', False, False)
