# Only run this file after setting mysql.db.table.suffix and other variables
# in mysql_config.R.
if grep -q "mytablesuffix" mysql_config.R ; then
    echo "Set mysql.db.table.suffix in mysql_config.R first."
    exit 1
fi

mysql_db_name=$(grep "mysql\.db\.name" mysql_config.R | tail -1 | sed 's/mysql\.db\.name <- "\([^"]*\)"/\1/')
mysql_user=$(grep "mysql\.user" mysql_config.R | tail -1 | sed 's/mysql\.user <- "\([^"]*\)"/\1/')
mysql_password=$(grep "mysql\.password" mysql_config.R | tail -1 | sed 's/mysql\.password <- "\([^"]*\)"/\1/')
table_suffix=$(grep "mysql\.db\.table\.suffix" mysql_config.R | tail -1 | sed 's/mysql\.db\.table\.suffix <- "\([^"]*\)"/\1/')

table_names=( "chars" "xp" "secret" )
cp create_schema.sql create_schema_populated.sql
for table_name in ${table_names[@]}; do
    sed -i -e 's/`'"${table_name}"'`'"/${table_name}_${table_suffix}/g" create_schema_populated.sql
done

echo Creating $mysql_db_name..$table_suffix...
mysql -u $mysql_user -p$mysql_password -D $mysql_db_name < create_schema_populated.sql

rm create_schema_populated.sql

