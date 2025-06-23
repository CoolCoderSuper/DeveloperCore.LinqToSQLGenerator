Imports System.IO
Imports System.Text
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.VisualBasic
'TODO: Table functions
'TODO: Make type generators share implementation
<Generator(LanguageNames.VisualBasic)>
Public Class LinqToSqlGenerator
    Implements IIncrementalGenerator

    Private Shared commonTypeMappings As New Dictionary(Of String, String) From {
        {"System.Int16", "Short"},
        {"System.Int32", "Integer"},
        {"System.Int64", "Long"},
        {"System.String", "String"},
        {"System.Boolean", "Boolean"},
        {"System.Single", "Single"},
        {"System.Double", "Double"},
        {"System.Decimal", "Decimal"},
        {"System.DateTime", "Date"},
        {"System.Date", "Date"},
        {"System.Guid", "Guid"},
        {"System.Byte", "Byte"},
        {"System.Byte[]", "Byte()"},
        {"System.Char", "Char"},
        {"System.SByte", "SByte"},
        {"System.UInt16", "UShort"},
        {"System.UInt32", "UInteger"},
        {"System.UInt64", "ULong"},
        {"System.Object", "Object"},
        {"System.TimeSpan", "TimeSpan"},
        {"System.DateTimeOffset", "DateTimeOffset"}
        }

    Public Sub Initialize(initContext As IncrementalGeneratorInitializationContext) Implements IIncrementalGenerator.Initialize
        Dim files As IncrementalValuesProvider(Of (String, String)) = initContext.AdditionalTextsProvider.Where(Function(x) x.Path.EndsWith(".dbml")).Select(Function(x, y) (Path.GetFileNameWithoutExtension(x.Path), x.GetText(y).ToString()))
        initContext.RegisterSourceOutput(files.Combine(initContext.CompilationProvider), AddressOf Generate)
    End Sub

    Private Sub Generate(context As SourceProductionContext, info As (Left As (String, String), Right As Compilation))
        Dim fileName As String = info.Left.Item1
        Dim s As String = info.Left.Item2
        Dim comp As Compilation = info.Right
        Dim doc As XDocument = XDocument.Parse(s)
        Dim db As XElement = doc.Root
        Dim sbContext As New StringBuilder
        sbContext.AppendLine("Imports System.Data")
        sbContext.AppendLine("Imports System.Reflection")
        sbContext.AppendLine("Imports System.Data.Linq")
        sbContext.AppendLine("Imports System.Data.Linq.Mapping")
        Dim contextNamespace As String = db.Attribute("ContextNamespace")?.Value
        Dim entityNamespace As String = db.Attribute("EntityNamespace")?.Value
        Dim className As String = db.Attribute("Class")?.Value
        Dim dbName As String = db.Attribute("Name")?.Value
        Dim accessModifier As String = GetAccessModifier(db.Attribute("AccessModifier")?.Value)
        Dim classModifier As String = GetClassModifier(db.Attribute("Modifier")?.Value)
        Dim baseType As String = If(db.Attribute("BaseType")?.Value, "DataContext")
        Dim provider As String = db.Attribute("Provider")?.Value
        Dim externalMapping As Boolean = If(db.Attribute("ExternalMapping")?.Value, False)
        Dim entityBase As String = db.Attribute("EntityBase")?.Value
        If contextNamespace IsNot Nothing Then sbContext.AppendLine($"Namespace {contextNamespace}")
        If dbName IsNot Nothing Then sbContext.AppendLine($"<Database(Name:=""{db.Attribute("Name").Value}"")>")
        If provider IsNot Nothing Then sbContext.AppendLine($"<Provider(GetType({provider}))>")
        If className Is Nothing AndAlso dbName Is Nothing Then
            context.ReportDiagnostic(Diagnostic.Create(New DiagnosticDescriptor("DBML001", "Missing class and name attribute", "Missing class and name attribute", "DBML", DiagnosticSeverity.Error, True), Location.None))
            Return
        End If
        sbContext.AppendLine($"{accessModifier} Partial {classModifier} Class {If(className, dbName)}")
        sbContext.AppendLine($"Inherits {baseType}")
        If Not externalMapping Then
            sbContext.AppendLine("Private Shared mappings As New AttributeMappingSource()")
            sbContext.AppendLine("Public Sub New(connection As String)")
            sbContext.AppendLine("MyBase.New(connection, mappings)")
            sbContext.AppendLine("End Sub")
            sbContext.AppendLine("Public Sub New(connection As IDbConnection)")
            sbContext.AppendLine("MyBase.New(connection, mappings)")
            sbContext.AppendLine("End Sub")
        End If
        sbContext.AppendLine("Public Sub New(connection As String, mappingSource As MappingSource)")
        sbContext.AppendLine("MyBase.New(connection, mappingSource)")
        sbContext.AppendLine("End Sub")
        sbContext.AppendLine("Public Sub New(connection As IDbConnection, mappingSource As MappingSource)")
        sbContext.AppendLine("MyBase.New(connection, mappingSource)")
        sbContext.AppendLine("End Sub")
        Dim types As XElement() = db.Descendants().Where(Function(x) x.Name.LocalName = "Type").ToArray
        For Each table As XElement In db.Elements().Where(Function(x) x.Name.LocalName = "Table")
            Dim type As XElement = table.FirstNode
            Dim associations As XElement() = type.Elements().Where(Function(x) x.Name.LocalName = "Association").ToArray
            Dim columns As XElement() = type.Elements().Where(Function(x) x.Name.LocalName = "Column").ToArray
            Dim orgTypeName As String = type.Attribute("Name").Value
            Dim typeName As String = GetSafeName(orgTypeName)
            Dim fullTypeName As String = $"{If(entityNamespace Is Nothing, "", $"{entityNamespace}.")}{typeName}"
            Dim memberAccess As String = GetAccessModifier(table.Attribute("AccessModifier")?.Value)
            Dim memberModifier As String = GetMemberModifier(table.Attribute("Modifier")?.Value)
            Dim inheritanceCode As String = type.Attribute("InheritanceCode")?.Value
            Dim isDefaultInheritance As Boolean = If(type.Attribute("IsInheritanceDefault")?.Value, False)
            Dim typeAccess As String = GetAccessModifier(type.Attribute("AccessModifier")?.Value)
            Dim typeModifier As String = GetClassModifier(type.Attribute("Modifier")?.Value)
            sbContext.AppendLine($"{memberAccess} {memberModifier} ReadOnly Property {table.Attribute("Member").Value} As Table(Of {fullTypeName})")
            sbContext.AppendLine("Get")
            sbContext.AppendLine($"Return GetTable(Of {fullTypeName})()")
            sbContext.AppendLine("End Get")
            sbContext.AppendLine("End Property")
            Dim sbType As New StringBuilder
            sbType.AppendLine("Imports System.ComponentModel")
            sbType.AppendLine("Imports System.Data.Linq")
            sbType.AppendLine("Imports System.Data.Linq.Mapping")
            If entityNamespace IsNot Nothing Then sbType.AppendLine($"Namespace {entityNamespace}")
            sbType.AppendLine($"<Table(Name:=""{table.Attribute("Name").Value}"")>")
            If inheritanceCode IsNot Nothing Then
                sbType.AppendLine($"<InheritanceMapping(Code:=""{inheritanceCode}"", Type:=GetType({typeName}), IsDefault:= {isDefaultInheritance})>")
            End If
            sbType.AppendLine($"{typeAccess} {typeModifier} Partial Class {typeName}")
            If entityBase IsNot Nothing Then sbType.AppendLine($"Inherits {entityBase}")
            sbType.AppendLine("Implements INotifyPropertyChanging, INotifyPropertyChanged")
            For Each column As XElement In columns
                Dim columnName As String = column.Attribute("Name").Value
                Dim memberName As String = If(column.Attribute("Member")?.Value, columnName)
                Dim memberNameSafe As String = GetSafeName(memberName)
                Dim isNullable As Boolean = If(column.Attribute("CanBeNull")?.Value, False)
                Dim isReadOnly As Boolean = If(column.Attribute("IsReadOnly")?.Value, False)
                Dim isDelayLoaded As Boolean = If(column.Attribute("IsDelayLoaded")?.Value, False)
                Dim fieldName As String = If(column.Attribute("Storage")?.Value, $"_{memberName}")
                Dim fieldValueAccessor As String = If(isDelayLoaded, ".Value", "")
                Dim columnTypeFull As String = column.Attribute("Type").Value
                Dim columnType As String = GetTypeName(columnTypeFull)
                Dim columnSymbol As INamedTypeSymbol = comp.GetTypeByMetadataName(columnTypeFull)
                Dim isNullValueType As Boolean = If(columnSymbol?.IsValueType, False) AndAlso isNullable
                If isNullValueType Then
                    columnType &= "?"
                End If
                Dim fieldType As String = If(isDelayLoaded, $"Link(Of {columnType})", columnType)
                Dim columnAccess As String = GetAccessModifier(column.Attribute("AccessModifier")?.Value)
                Dim columnModifier As String = GetMemberModifier(column.Attribute("Modifier")?.Value)
                sbType.AppendLine($"Private {fieldName} As {fieldType}")
                sbType.AppendLine(GetColumnAttributes(column))
                sbType.AppendLine($"{columnAccess} {columnModifier} {If(isReadOnly, "ReadOnly", "")} Property {memberNameSafe} As {columnType}")
                sbType.AppendLine("Get")
                sbType.AppendLine($"Return {fieldName}{fieldValueAccessor}")
                sbType.AppendLine("End Get")
                If Not isReadOnly Then
                    sbType.AppendLine("Set")
                    If Not isNullValueType AndAlso columnSymbol IsNot Nothing AndAlso columnSymbol.IsValueType Then
                        sbType.AppendLine($"If {fieldName}{fieldValueAccessor} <> Value Then")
                    Else
                        sbType.AppendLine($"If Not Object.Equals({fieldName}{fieldValueAccessor}, Value) Then")
                    End If
                    Dim colAssociations As XElement() = associations.Where(Function(x) x.Attribute("ThisKey").Value = columnName AndAlso If(x.Attribute("IsForeignKey")?.Value, False)).ToArray
                    For Each association As XElement In colAssociations
                        Dim member As String = $"_{association.Attribute("Member").Value}"
                        sbType.AppendLine($"If {member}.HasLoadedOrAssignedValue Then")
                        sbType.AppendLine("Throw New ForeignKeyReferenceAlreadyHasValueException()")
                        sbType.AppendLine("End If")
                    Next
                    sbType.AppendLine($"RaiseEvent PropertyChanging(Me, New PropertyChangingEventArgs(""{memberNameSafe}""))")
                    sbType.AppendLine($"{fieldName}{fieldValueAccessor} = Value")
                    sbType.AppendLine($"RaiseEvent PropertyChanged(Me, New PropertyChangedEventArgs(""{memberNameSafe}""))")
                    sbType.AppendLine("End If")
                    sbType.AppendLine("End Set")
                End If
                sbType.AppendLine("End Property")
            Next
            Dim sbConstructor As New StringBuilder
            For Each association As XElement In associations
                Dim member As String = association.Attribute("Member").Value
                Dim memberSafe As String = GetSafeName(member)
                Dim fieldName As String = If(association.Attribute("Storage")?.Value, $"_{member}")
                Dim orgType As String = association.Attribute("Type").Value
                Dim typeRef As String = GetSafeName(GetTypeName(orgType))
                Dim isFK As Boolean = If(association.Attribute("IsForeignKey")?.Value, False)
                Dim name As String = association.Attribute("Name").Value
                Dim parentKey As String = association.Attribute("ThisKey").Value
                Dim childKey As String = GetSafeName(association.Attribute("OtherKey").Value)
                Dim fullType As String = $"{If(isFK, "EntityRef", "EntitySet")}(Of {typeRef})"
                sbType.AppendLine($"Private {fieldName} As {fullType}")
                Dim sbAttributes As New StringBuilder
                sbAttributes.Append($"<Association(Name:=""{name}"", Storage:=""{fieldName}""")
                sbAttributes.Append($", ThisKey:=""{parentKey}"", OtherKey:=""{childKey}"", IsForeignKey:={isFK})>")
                sbType.AppendLine(sbAttributes.ToString())
                sbType.AppendLine($"Public Property {memberSafe} As {If(isFK, typeRef, fullType)}")
                sbType.AppendLine("Get")
                If isFK Then
                    sbType.AppendLine($"Return {fieldName}.Entity")
                Else
                    sbType.AppendLine($"Return {fieldName}")
                End If
                sbType.AppendLine("End Get")
                sbType.AppendLine("Set")
                Dim parentType As XElement = types.First(Function(x) x.Attribute("Name").Value = orgType)
                Dim childAssociation As XElement = parentType.Elements().First(Function(x) x.Name.LocalName = "Association" AndAlso x.Attribute("Type").Value = orgTypeName)
                Dim childMember As String = GetSafeName(childAssociation.Attribute("Member").Value)
                Dim parentKeyColumn As XElement = columns.First(Function(x) x.Attribute("Name").Value = parentKey OrElse x.Attribute("Member")?.Value = parentKey)
                Dim parentKeyMember As String = If(parentKeyColumn.Attribute("Storage")?.Value, $"_{If(parentKeyColumn.Attribute("Member")?.Value, parentKeyColumn.Attribute("Name").Value)}")
                If isFK Then
                    sbType.AppendLine($"Dim previousValue As {typeRef} = {fieldName}.Entity")
                    sbType.AppendLine($"If Not Object.Equals(previousValue, Value) OrElse Not {fieldName}.HasLoadedOrAssignedValue Then")
                    sbType.AppendLine($"RaiseEvent PropertyChanging(Me, New PropertyChangingEventArgs(""{memberSafe}""))")
                    sbType.AppendLine("If previousValue IsNot Nothing Then")
                    sbType.AppendLine($"{fieldName}.Entity = Nothing")
                    sbType.AppendLine($"previousValue.{childMember}.Remove(Me)")
                    sbType.AppendLine("End If")
                    sbType.AppendLine($"{fieldName}.Entity = Value")
                    sbType.AppendLine("If Value IsNot Nothing Then")
                    sbType.AppendLine($"Value.{childMember}.Add(Me)")
                    sbType.AppendLine($"{parentKeyMember} = Value.{childKey}")
                    sbType.AppendLine("End If")
                    sbType.AppendLine($"RaiseEvent PropertyChanged(Me, New PropertyChangedEventArgs(""{memberSafe}""))")
                    sbType.AppendLine("End If")
                Else
                    sbType.AppendLine($"{fieldName}.Assign(Value)")
                End If
                sbType.AppendLine("End Set")
                sbType.AppendLine("End Property")
                If Not isFK Then
                    sbConstructor.AppendLine($"{fieldName} = New {fullType}(AddressOf attach{fieldName}, AddressOf detach{fieldName})")
                    sbType.AppendLine($"Private Sub attach{fieldName}(entity As {typeRef})")
                    sbType.AppendLine($"RaiseEvent PropertyChanging(Me, New PropertyChangingEventArgs(""{memberSafe}""))")
                    sbType.AppendLine($"entity.{childMember} = Me")
                    sbType.AppendLine("End Sub")
                    sbType.AppendLine($"Private Sub detach{fieldName}(entity As {typeRef})")
                    sbType.AppendLine($"RaiseEvent PropertyChanging(Me, New PropertyChangingEventArgs(""{memberSafe}""))")
                    sbType.AppendLine($"entity.{childMember} = Nothing")
                    sbType.AppendLine("End Sub")
                End If
            Next
            sbType.AppendLine("Public Event PropertyChanging As PropertyChangingEventHandler Implements INotifyPropertyChanging.PropertyChanging")
            sbType.AppendLine("Public Event PropertyChanged As PropertyChangedEventHandler Implements INotifyPropertyChanged.PropertyChanged")
            sbType.AppendLine("Public Sub New()")
            sbType.Append(sbConstructor.ToString)
            sbType.AppendLine("End Sub")
            sbType.AppendLine("End Class")
            If entityNamespace IsNot Nothing Then sbType.AppendLine("End Namespace")
            context.AddSource(typeName & ".g.vb", SyntaxFactory.ParseCompilationUnit(sbType.ToString).NormalizeWhitespace.ToFullString)
        Next
        For Each func As XElement In db.Elements().Where(Function(x) x.Name.LocalName = "Function")
            Dim funcName As String = func.Attribute("Name").Value
            Dim methodName As String = GetSafeName(func.Attribute("Method").Value)
            Dim type As XElement = func.Elements().First(Function(x) x.Name.LocalName = "ElementType")
            Dim typeId As String = type.Attribute("IdRef")?.Value
            Dim typeName As String
            If typeId Is Nothing Then
                typeName = GetSafeName(type.Attribute("Name").Value)
            Else
                typeName = GetSafeName(types.First(Function(x) x.Attribute("Id")?.Value = typeId).Attribute("Name").Value)
            End If
            Dim columns As XElement() = type.Elements().Where(Function(x) x.Name.LocalName = "Column").ToArray()
            Dim params As XElement() = func.Elements().Where(Function(x) x.Name.LocalName = "Parameter").ToArray()
            Dim sbFuncParams As New StringBuilder()
            For Each param As XElement In params
                Dim name As String = param.Attribute("Name").Value
                Dim dbType As String = param.Attribute("DbType").Value
                Dim paramName As String = GetSafeName(If(param.Attribute("Parameter")?.Value, name))
                Dim paramType As String = GetTypeName(param.Attribute("Type").Value)
                sbFuncParams.Append($"<Parameter(Name:=""{name}"", DbType:=""{dbType}"")>{paramName} As {paramType}")
                If param IsNot params.Last Then sbFuncParams.Append(", ")
            Next
            Dim sbCallValues As New StringBuilder()
            For Each param As XElement In params
                sbCallValues.Append($", {GetSafeName(If(param.Attribute("Parameter")?.Value, param.Attribute("Name").Value))}")
            Next
            sbContext.AppendLine($"<[Function](Name:=""{funcName}"")>")
            sbContext.AppendLine($"Public Function {methodName}({sbFuncParams}) As ISingleResult(Of {typeName})")
            sbContext.AppendLine($"Dim result As IExecuteResult = ExecuteMethodCall(Me, MethodInfo.GetCurrentMethod(){sbCallValues})")
            sbContext.AppendLine($"Return CType(result.ReturnValue, ISingleResult(Of {typeName}))")
            sbContext.AppendLine("End Function")
            If typeId Is Nothing Then
                Dim sbType As New StringBuilder
                sbType.AppendLine("Imports System.Data.Linq.Mapping")
                If entityNamespace IsNot Nothing Then sbType.AppendLine($"Namespace {entityNamespace}")
                sbType.AppendLine($"Public Partial Class {typeName}")
                For Each column As XElement In columns
                    Dim columnName As String = column.Attribute("Name").Value
                    Dim memberName As String = If(column.Attribute("Member")?.Value, columnName)
                    Dim memberNameSafe As String = GetSafeName(memberName)
                    Dim columnAccess As String = GetAccessModifier(column.Attribute("AccessModifier")?.Value)
                    Dim columnModifier As String = GetMemberModifier(column.Attribute("Modifier")?.Value)
                    Dim isReadOnly As Boolean = If(column.Attribute("IsReadOnly")?.Value, False)
                    Dim isNullable As Boolean = If(column.Attribute("CanBeNull")?.Value, False)
                    Dim columnTypeFull As String = column.Attribute("Type").Value
                    Dim columnType As String = GetTypeName(columnTypeFull)
                    Dim columnSymbol As INamedTypeSymbol = comp.GetTypeByMetadataName(columnTypeFull)
                    If columnSymbol?.IsValueType AndAlso isNullable Then
                        columnType &= "?"
                    End If
                    sbType.AppendLine(GetColumnAttributes(column))
                    sbType.AppendLine($"{columnAccess} {columnModifier} {If(isReadOnly, "ReadOnly", "")} Property {memberNameSafe} As {columnType}")
                Next
                sbType.AppendLine("End Class")
                If entityNamespace IsNot Nothing Then sbType.AppendLine("End Namespace")
                context.AddSource(typeName & ".g.vb", SyntaxFactory.ParseCompilationUnit(sbType.ToString).NormalizeWhitespace.ToFullString)
            End If
        Next
        sbContext.AppendLine("End Class")
        If contextNamespace IsNot Nothing Then sbContext.AppendLine("End Namespace")
        context.AddSource(fileName & ".g.vb", SyntaxFactory.ParseCompilationUnit(sbContext.ToString).NormalizeWhitespace.ToFullString)
    End Sub

    Private Shared Function GetColumnAttributes(column As XElement) As String
        Dim columnName As String = column.Attribute("Name").Value
        Dim memberName As String = If(column.Attribute("Member")?.Value, columnName)
        Dim fieldName As String = If(column.Attribute("Storage")?.Value, $"_{memberName}")
        Dim isNullable As Boolean = If(column.Attribute("CanBeNull")?.Value, False)
        Dim isPrimaryKey As Boolean = If(column.Attribute("IsPrimaryKey")?.Value, False)
        Dim isVersion As Boolean = If(column.Attribute("IsVersion")?.Value, False)
        Dim isDiscriminator As Boolean = If(column.Attribute("IsDiscriminator")?.Value, False)
        Dim isDbGeneratedValue As String = column.Attribute("IsDbGenerated")?.Value
        If isVersion AndAlso isDbGeneratedValue Is Nothing Then
            isDbGeneratedValue = "True"
        End If
        Dim isDbGenerated As Boolean = If(isDbGeneratedValue, False)
        Dim updateCheck As String = column.Attribute("UpdateCheck")?.Value
        Dim dbType As String = column.Attribute("DbType").Value
        Dim autoSync As String = column.Attribute("AutoSync")?.Value
        Dim expression As String = column.Attribute("Expression")?.Value
        Dim sbAttributes As New StringBuilder
        sbAttributes.Append($"<Column(Storage:=""{fieldName}"", CanBeNull:={isNullable}")
        If dbType IsNot Nothing Then
            sbAttributes.Append($", DbType:=""{dbType}""")
        End If
        If autoSync IsNot Nothing Then
            sbAttributes.Append($", AutoSync:=AutoSync.{autoSync}")
        End If
        If isDbGenerated Then
            If autoSync Is Nothing Then
                If isPrimaryKey Then
                    sbAttributes.Append($", AutoSync:=AutoSync.OnInsert")
                Else
                    sbAttributes.Append($", AutoSync:=AutoSync.Always")
                End If
            End If
            sbAttributes.Append($", IsDbGenerated:={isDbGenerated}")
        End If
        If isPrimaryKey Then
            sbAttributes.Append($", IsPrimaryKey:={isPrimaryKey}")
        End If
        If isDiscriminator Then
            sbAttributes.Append($", IsDiscriminator:={isDiscriminator}")
        End If
        If isVersion Then
            sbAttributes.Append($", IsVersion:={isVersion}")
        End If
        If updateCheck <> Nothing Then
            sbAttributes.Append($", UpdateCheck:=UpdateCheck.{updateCheck}")
        End If
        If expression <> Nothing Then
            sbAttributes.Append($", Expression:=""{expression}""")
        End If
        If columnName <> memberName Then
            sbAttributes.Append($", Name:=""{columnName}""")
        End If
        sbAttributes.Append(")>")
        Return sbAttributes.ToString()
    End Function

    Private Shared Function GetAccessModifier(modifier As String) As String
        If modifier = "Internal" Then
            Return "Friend"
        ElseIf modifier = "Protected" Then
            Return "Protected"
        ElseIf modifier = "ProtectedInternal" Then
            Return "Protected Friend"
        End If
        Return If(modifier, "Public")
    End Function

    Private Shared Function GetClassModifier(modifier As String) As String
        If modifier = "Sealed" Then
            Return "NotInheritable"
        ElseIf modifier = "Abstract" Then
            Return "MustInherit"
        End If
        Return ""
    End Function

    Private Shared Function GetMemberModifier(modifier As String) As String
        If modifier = "Virtual" Then
            Return "Overridable"
        ElseIf modifier = "Override" Then
            Return "Overrides"
        ElseIf modifier = "New" Then
            Return "Shadows"
        ElseIf modifier = "NewVirtual" Then
            Return "Overridable Shadows"
        End If
        Return ""
    End Function

    Private Shared Function GetTypeName(name As String) As String
        Dim tempName As String = name
        If commonTypeMappings.TryGetValue(name, tempName) Then Return tempName
        Return name
    End Function

    Private Shared Function GetSafeName(name As String) As String
        Dim token As SyntaxToken = SyntaxFactory.ParseToken(name)
        If token.IsReservedKeyword() Then
            Return $"[{name}]"
        End If
        Return name
    End Function
End Class